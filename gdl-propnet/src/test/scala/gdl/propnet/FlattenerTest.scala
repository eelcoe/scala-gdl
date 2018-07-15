package gdl.propnet

import gdl.lang._
import org.scalacheck.Prop.{BooleanOperators, forAll, throws}
import org.scalacheck.{Gen, Prop, Properties}
import util.CustomGenerators
import util.ObjectGenerators

class FlattenerTest extends Properties("Flatten_GDL") {
  import CustomGenerators._
  import ObjectGenerators._
  import Flattener.flatten

  private def inits(argsGen: Gen[Term]) = for {
    term <- argsGen
  } yield Rule(Init(term), Seq())

  property("flatten_safe_rules") = Prop.forAllNoShrink(Gen.listOfN(10, safeRules)) { rules =>
    flatten(Description(rules))
    true
  }

  property("ignore_uninitialized") = forAll(termsContaining(Set(Variable("x")))) { term =>
    val description = Description(rules = Seq(
      Rule(Terminal(), Seq(True(term)))
    ))

    flatten(description).rules.size == 0
  }

  property("retain_inits") = forAll(Gen.listOf(inits(termsContaining(Set())))) { inits =>
    val description = Description(inits)

    flatten(description).rules.size == inits.toSet.size
  }

  property("substitute_simple_terms") = forAll(constants, variables) { (constant, variable) =>
    val description = Description(Seq(
      Rule(Init(constant), Seq()),
      Rule(Terminal(), Seq(True(variable)))
    ))

    flatten(description).rules.size == 2
  }

  property("substitute_functions") = forAll(Gen.identifier, constants, variables) { (name, constant, variable) =>
    val description = Description(Seq(
      Rule(Init(AppliedFunction(name, Seq(constant))), Seq()),
      Rule(Terminal(), Seq(True(AppliedFunction(name, Seq(variable)))))
    ))

    flatten(description).rules.size == 2
  }

  property("ignore_incompatible_inits") = forAll(functions(constants), variables) { (function, variable) =>
    val description = Description(Seq(
      Rule(Init(function), Seq()),
      Rule(Terminal(), Seq(True(variable)))
    ))

    flatten(description).rules.size == 1
  }

  property("ignore_unreachable_inits") = forAll(constants, variables) { (constant, variable) =>
    val description = Description(Seq(
      Rule(Init(constant), Seq(AtomicSentence("foo", Seq(variable)))),
      Rule(Terminal(), Seq(True(constant)))
    ))

    flatten(description).rules.size == 0
  }

  property("substitute_multiple_inits") = forAll(Gen.nonEmptyListOf(inits(constants)), variables) { (inits, variable) =>
    val description = Description(inits ++ Seq(
      Rule(Terminal(), Seq(True(variable)))
    ))

    flatten(description).rules.size == 2 * inits.toSet.size
  }

  property("introduce_object_constants") = forAll(Gen.nonEmptyListOf(inits(constants)), constants, variables) { (inits, constant, variable) =>
    val description = Description(inits ++ Seq(
      Rule(Next(constant), Seq(True(variable)))
    ))

    !inits.flatMap(_.head.terms).contains(constant) ==> (flatten(description).rules.size == 2 * inits.toSet.size + 1)
  }

  property("multiply_object_constants") = forAll(nonEmptyListOfMax(10, inits(constants)), functions(variables)) { (inits, function) =>
    val variables = function.collectVariables.toSeq
    val description = Description(inits ++ Seq(
      Rule(Next(function), variables.map(True.apply))
    ))

    val initsSetSize = inits.toSet.size
    flatten(description).rules.size == initsSetSize + Math.pow(initsSetSize, variables.size)
  }

  property("match_object_constants") = forAll(constants, variables, variables, variables) { (constant, var1, var2, var3) =>
    val description = Description(Seq(
      Rule(Init(constant), Seq()),
      Rule(Legal(var1, var2), Seq(True(var1), True(var2))),
      Rule(Next(var3), Seq(Does(constant, var3)))
    ))

    flatten(description).rules.size == 3
  }

  property("retain_distinct_with_equal_constants") = forAll(atomicSentences(constants), termsContaining(Set())) { (head, term) =>
    val description = Description(Seq(Rule(head, Seq(Distinct(term, term)))))

    flatten(description).rules.size == 1
  }

  property("include_unreachable_object_constants") = forAll(Gen.nonEmptyListOf(inits(constants)), constants, variables) { (inits, constant, variable) =>
    val description = Description(inits ++ Seq(
      Rule(Next(constant), Seq(True(variable), Distinct(constant, variable)))
    ))

    !inits.flatMap(_.head.terms).contains(constant) ==> (flatten(description).rules.size == 2 * inits.toSet.size + 1)
  }

  property("deal_with_unstratified_rules") = forAll(constants, variables) { (constant, variable) =>
    val description = Description(Seq(
      Rule(Init(constant), Seq()),
      Rule(AtomicSentence("foo", Seq(variable)), Seq(True(variable), Not(AtomicSentence("foo", Seq(variable)))))
    ))

    flatten(description).rules.size == 2
  }

}
