package gdl.parsing

import gdl.lang._
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.{AnyOperators, forAll}
import scala.util.Success
import util.NameGenerators

class InfixGdlParserTest extends Properties("Infix_GDL") {
  import NameGenerators._
  import InfixGdlParser.parseAs

  implicit val termParser = InfixGdlParser.term
  implicit val literalParser = InfixGdlParser.literal
  implicit val ruleParser = InfixGdlParser.rule

  def variables = for {
    c1 <- Gen.alphaUpperChar
    s <- Gen.alphaNumStr
  } yield c1 + s

  def comments = Gen.listOf(Arbitrary.arbitrary[Char].suchThat(_ != '\u001a')).map("/*" + _.mkString + "*/")

  val makeFunctionString = (name: String, args: Seq[String]) => name + "(" + args.mkString(",") + ")"
  def makeFunctionObject(argConstructor: String => Term) =
    (name: String, args: Seq[String]) => AppliedFunction(name, args.map(argConstructor))

  property("object_constants") = forAll(constants) { (name: String) =>
    parseAs[Term](name) ?= Success(ObjectConstant(name))
  }

  property("variables") = forAll(variables) { (name: String) =>
    parseAs[Term](name) ?= Success(Variable(name))
  }

  property("functions_with_constant_arguments") = forAll(functions(constants)) { case (name: String, args: Seq[String]) =>
    parseAs[Term](makeFunctionString(name, args)) ?= Success(AppliedFunction(name, args.map(ObjectConstant.apply)))
  }

  property("functions_with_variable_arguments") = forAll(functions(variables)) { case (name: String, args: Seq[String]) =>
    parseAs[Term](makeFunctionString(name, args)) ?= Success(AppliedFunction(name, args.map(Variable.apply)))
  }

  property("nested_functions") = forAll(nestedFunctions) { case (name: String, args: Seq[(String, Seq[String])]) =>
    parseAs[Term](makeFunctionString(name, args.map(makeFunctionString.tupled))) ?=
      Success(AppliedFunction(name, args.map(makeFunctionObject(ObjectConstant.apply).tupled)))
  }

  property("relation_with_zero_arity") = forAll(Gen.identifier) { name: String =>
    parseAs[Literal](name) ?= Success(AtomicSentence(name))
  }

  property("relations_with_constant_arguments") = forAll(functions(constants)) { case (name: String, args: Seq[String]) =>
    parseAs[Literal](makeFunctionString(name, args)) ?= Success(AtomicSentence(name, args.map(ObjectConstant.apply)))
  }

  property("relations_with_variable_arguments") = forAll(functions(variables)) { case (name: String, args: Seq[String]) =>
    parseAs[Literal](makeFunctionString(name, args)) ?= Success(AtomicSentence(name, args.map(Variable.apply)))
  }

  property("relations_with_function_arguments") = forAll(nestedFunctions) { case (name: String, args: Seq[(String, Seq[String])]) =>
    parseAs[Literal](makeFunctionString(name, args.map(makeFunctionString.tupled))) ?=
      Success(AtomicSentence(name, args.map(makeFunctionObject(ObjectConstant.apply).tupled)))
  }

  property("negated_relations") = forAll(functions(constants)) { case (name: String, args: Seq[String]) =>
    parseAs[Literal]("~" + makeFunctionString(name, args)) ?=
      Success(Not(AtomicSentence(name, args.map(ObjectConstant.apply))))
  }

  property("rules_without_bodies") = forAll(functions(constants)) { case (name: String, args: Seq[String]) =>
    parseAs[Rule](makeFunctionString(name, args)) ?=
      Success(Rule(AtomicSentence(name, args.map(ObjectConstant.apply)), Seq()))
  }

  property("rules_with_bodies") = forAll(rules(Gen.identifier, Gen.identifier)) { case (head: String, body: Seq[String]) =>
    parseAs[Rule](head + ":-" + body.mkString("&")) ?=
      Success(Rule(AtomicSentence(head, Seq()), body.map(AtomicSentence.apply)))
  }

  property("multiple_rules") = forAll(Gen.listOf(rules(Gen.identifier, Gen.identifier))) { rules: Seq[(String, Seq[String])] =>
    InfixGdlParser((rules.map { case (head: String, body: Seq[String]) => head + ":-" + body.mkString("&") }).mkString("\n")) ?=
      Success(Description(rules.map { case (head: String, body: Seq[String]) => Rule(AtomicSentence(head), body.map(AtomicSentence.apply)) }))
  }

  property("ignore_empty_lines_and_comments") = forAll(Gen.listOf(Gen.oneOf(Gen.const(""), comments, Gen.identifier))) { lines: Seq[String] =>
    InfixGdlParser(lines.mkString("\n")) ?=
      Success(Description(lines.flatMap { line =>
        if (line.size == 0 || line.startsWith("/*")) None else Some(Rule(AtomicSentence(line), Seq()))
      }))
  }

}