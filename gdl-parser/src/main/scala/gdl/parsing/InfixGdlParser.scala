package gdl.parsing

import gdl.lang._
import gdl.parsing.parser.GdlParser

import scala.util.Try

object InfixGdlParser extends GdlParser {
  lexical.delimiters ++= Set("(", ")", ",", "~", ":-", "&")
  lexical.reserved ++= Set("distinct")

  private val startsWithLowerCase = { s: String => s.charAt(0).isLower }
  private val startsWithUpperCase = { s: String => s.charAt(0).isUpper }
  private val isInteger = { s: String => !s.contains('.') }
  private val identLowerCase = ident filter startsWithLowerCase

  def objectConstant = (identLowerCase | (numericLit filter isInteger)) ^^ (ObjectConstant.apply)
  def variable = (ident filter startsWithUpperCase) ^^ (Variable.apply)

  private def arguments = "(" ~> rep1sep(term, ",") <~ ")"

  private def functionConstant = identLowerCase ^^ (name => { arity: Int => FunctionConstant(name, arity) })
  private def appliedFunction = functionConstant ~ arguments ^^ {
    case functionOfArity ~ args => AppliedFunction(functionOfArity(args.size), args)
  }

  def term: Parser[Term] = appliedFunction | objectConstant | variable

  private def relationConstant = identLowerCase ^^ (name => { arity: Int => RelationConstant(name, arity) })
  private def appliedRelation = relationConstant ~ arguments ^^ {
    case relationOfArity ~ args => AtomicSentence(relationOfArity(args.size), args)
  }
  private def zeroArityRelation = identLowerCase ^^ (name => AtomicSentence(RelationConstant(name, 0), Seq()))
  private def atomicSentence = appliedRelation | zeroArityRelation
  private def negatedAtomicSentence = "~" ~> atomicSentence ^^ (Not.apply)
  private def distinct = "distinct" ~ "(" ~> term ~ "," ~ term <~ ")" ^^ {
    case x ~ _ ~ y => Distinct(x, y)
  }

  def literal: Parser[Literal] = atomicSentence | negatedAtomicSentence | distinct

  def rule: Parser[Rule] = (atomicSentence ~ (":-" ~> rep1sep(literal, "&")).?) ^^ {
    case head ~ None => Rule(head, Seq())
    case head ~ Some(body) => Rule(head, body)
  }

  def description: Parser[Description] = rule.* ^^ (Description.apply)

  def apply(text: String): Try[Description] = parseAs(text)(description)
}
