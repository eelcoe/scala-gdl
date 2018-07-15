package gdl.parsing

import gdl.lang._
import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.{Arbitrary, Gen, Properties}
import scala.util.Success
import util.NameGenerators

class PrefixGdlParserTest extends Properties("Prefix_GDL") {
  import NameGenerators._
  import PrefixGdlParser.parseAs

  implicit val termParser = PrefixGdlParser.term
  implicit val literalParser = PrefixGdlParser.literal
  implicit val ruleParser = PrefixGdlParser.rule

  def variables = Gen.identifier.map("?" + _)
  def comments = Gen.listOf(Arbitrary.arbitrary[Char].suchThat(!Set('\u001a', '\n').contains(_))).map(";" + _.mkString)

  val makeFunctionString = (name: String, args: Seq[String]) => "(" + (name +: args).mkString(" ") + ")"
  def makeFunctionObject(argConstructor: String => Term) =
    (name: String, args: Seq[String]) => AppliedFunction(name, args.map(argConstructor))

  property("object_constants") = forAll(constants) { (name: String) =>
    parseAs[Term](name) ?= Success(ObjectConstant(name))
  }

  property("variables") = forAll(variables) { (name: String) =>
    parseAs[Term](name) ?= Success(Variable(name.substring(1)))
  }

  property("functions_with_constant_arguments") = forAll(functions(constants)) { case (name: String, args: Seq[String]) =>
    parseAs[Term](makeFunctionString(name, args)) ?= Success(AppliedFunction(name, args.map(ObjectConstant.apply)))
  }

  property("functions_with_variable_arguments") = forAll(functions(variables)) { case (name: String, args: Seq[String]) =>
    parseAs[Term](makeFunctionString(name, args)) ?= Success(AppliedFunction(name, args.map(_.substring(1)).map(Variable.apply)))
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
    parseAs[Literal](makeFunctionString(name, args)) ?= Success(AtomicSentence(name, args.map(_.substring(1)).map(Variable.apply)))
  }

  property("relations_with_function_arguments") = forAll(nestedFunctions) { case (name: String, args: Seq[(String, Seq[String])]) =>
    parseAs[Literal](makeFunctionString(name, args.map(makeFunctionString.tupled))) ?=
      Success(AtomicSentence(name, args.map(makeFunctionObject(ObjectConstant.apply).tupled)))
  }

  property("negated_relations") = forAll(functions(constants)) { case (name: String, args: Seq[String]) =>
    parseAs[Literal]("(not " + makeFunctionString(name, args) + ")") ?=
      Success(Not(AtomicSentence(name, args.map(ObjectConstant.apply))))
  }

  property("distinct_constant_relations") = forAll(Gen.listOfN(2, constants)) { terms =>
    parseAs[Literal]("(distinct " + terms.mkString(" ") + ")") ?=
      Success(Distinct(ObjectConstant(terms(0)), ObjectConstant(terms(1))))
  }

  property("distinct_variable_relations") = forAll(Gen.listOfN(2, variables)) { terms =>
    parseAs[Literal]("(distinct " + terms.mkString(" ") + ")") ?=
      Success(Distinct(Variable(terms(0).substring(1)), Variable(terms(1).substring(1))))
  }

  property("rules_without_bodies") = forAll(functions(constants)) { case (name: String, args: Seq[String]) =>
    parseAs[Rule](makeFunctionString(name, args)) ?=
      Success(Rule(AtomicSentence(name, args.map(ObjectConstant.apply)), Seq()))
  }

  property("rules_with_bodies") = forAll(rules(Gen.identifier, Gen.identifier)) { case (head: String, body: Seq[String]) =>
    parseAs[Rule]("(<= " + (head +: body).mkString(" ") + ")") ?=
      Success(Rule(AtomicSentence(head), body.map(AtomicSentence.apply)))
  }

  property("multiple_rules") = forAll(Gen.listOf(rules(Gen.identifier, Gen.identifier))) { rules: Seq[(String, Seq[String])] =>
    PrefixGdlParser((rules.map { case (head: String, body: Seq[String]) => "(<= " + (head +: body).mkString(" ") + ")" }).mkString("\n")) ?=
      Success(Description(rules.map { case (head: String, body: Seq[String]) => Rule(AtomicSentence(head), body.map(AtomicSentence.apply)) }))
  }

  property("ignore_empty_lines_and_comments") = forAll(Gen.listOf(Gen.oneOf(Gen.const(""), comments, Gen.identifier))) { lines: Seq[String] =>
    PrefixGdlParser(lines.mkString("\n")) ?=
      Success(Description(lines.flatMap { line =>
        if (line.size == 0 || line.startsWith(";")) None else Some(Rule(AtomicSentence(line), Seq()))
      }))
  }

}
