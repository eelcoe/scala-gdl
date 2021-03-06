package gdl.parsing

import gdl.lang._
import gdl.parsing.parser.{GdlParser, SemiColonCommentLexical}

import scala.util.Try

object PrefixGdlParser extends GdlParser {
  override val lexical = new SemiColonCommentLexical
  lexical.delimiters ++= Set("(", ")", "?", "<=")
  lexical.reserved ++= Set("not", "distinct")

  private val isInteger = { s: String => !s.contains('.') }

  def objectConstant = (ident | (numericLit filter isInteger)) ^^ (ObjectConstant.apply)
  def variable = "?" ~> ident ^^ (Variable.apply)

  private def arguments = rep1(term)

  private def functionConstant = ident ^^ (name => { arity: Int => FunctionConstant(name, arity) })
  private def appliedFunction = "(" ~> functionConstant ~ arguments <~ ")" ^^ {
    case functionOfArity ~ args => AppliedFunction(functionOfArity(args.size), args)
  }

  def term: Parser[Term] = appliedFunction | objectConstant | variable

  private def relationConstant = ident ^^ (name => { arity: Int => RelationConstant(name, arity) })
  private def appliedRelation = "(" ~> relationConstant ~ arguments <~ ")" ^^ {
    case relationOfArity ~ args => AtomicSentence(relationOfArity(args.size), args)
  }
  private def zeroArityRelation = ident ^^ (name => AtomicSentence(RelationConstant(name, 0), Seq()))
  private def atomicSentence = appliedRelation | zeroArityRelation
  private def negatedAtomicSentence = "(" ~ "not" ~> atomicSentence <~ ")" ^^ (Not.apply)
  private def distinct = "(" ~ "distinct" ~> term ~ term <~ ")" ^^ {
    case x ~ y => Distinct(x, y)
  }

  def literal: Parser[Literal] = atomicSentence | negatedAtomicSentence | distinct

  private def fact = atomicSentence ^^ (Rule(_, Seq()))
  private def implication = "(" ~ "<=" ~> atomicSentence ~ rep1(literal) <~ ")" ^^ {
    case head ~ body => Rule(head, body)
  }
  def rule: Parser[Rule] = fact | implication

  def description: Parser[Description] = rule.* ^^ (Description.apply)

  def apply(text: String): Try[Description] = parseAs(text)(description)
}
