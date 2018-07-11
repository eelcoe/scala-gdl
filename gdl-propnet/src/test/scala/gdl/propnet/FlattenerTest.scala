package gdl.propnet

import gdl.lang._
import org.scalacheck.Prop.{BooleanOperators, forAll, throws}
import org.scalacheck.{Gen, Prop, Properties}
import util.ObjectGenerators

class FlattenerTest extends Properties("Flatten_GDL") {
  import ObjectGenerators._
  import Flattener.flatten

  private def inits(argsGen: Gen[Term]) = for {
    term <- argsGen
  } yield Rule(AtomicSentence("init", Seq(term)), Seq())

  property("flatten_safe_rules") = Prop.forAllNoShrink(Gen.listOfN(10, safeRules)) { rules =>
    flatten(Description(rules))
    true
  }

  property("ignore_uninitialized") = forAll(termsContaining(Set(Variable("x")))) { term =>
    val description = Description(rules = Seq(
      Rule(AtomicSentence("terminal"), Seq(AtomicSentence("true", Seq(term))))
    ))

    flatten(description).rules.size == 0
  }

  property("retain_inits") = forAll(Gen.listOf(inits(termsContaining(Set())))) { inits =>
    val description = Description(inits)

    flatten(description).rules.size == inits.toSet.size
  }

  property("substitute_simple_terms") = forAll(constants, variables) { (constant, variable) =>
    val description = Description(Seq(
      Rule(AtomicSentence("init", Seq(constant)), Seq()),
      Rule(AtomicSentence("terminal"), Seq(AtomicSentence("true", Seq(variable))))
    ))

    flatten(description).rules.size == 2
  }

  property("substitute_functions") = forAll(Gen.identifier, constants, variables) { (name, constant, variable) =>
    val description = Description(Seq(
      Rule(AtomicSentence("init", Seq(AppliedFunction(name, Seq(constant)))), Seq()),
      Rule(AtomicSentence("terminal"), Seq(AtomicSentence("true", Seq(AppliedFunction(name, Seq(variable))))))
    ))

    flatten(description).rules.size == 2
  }

  property("ignore_incompatible_inits") = forAll(functions(constants), variables) { (function, variable) =>
    val description = Description(Seq(
      Rule(AtomicSentence("init", Seq(function)), Seq()),
      Rule(AtomicSentence("terminal"), Seq(AtomicSentence("true", Seq(variable))))
    ))

    flatten(description).rules.size == 1
  }

  property("ignore_unreachable_init") = forAll(constants, variables) { (constant, variable) =>
    val description = Description(Seq(
      Rule(AtomicSentence("init", Seq(constant)), Seq(AtomicSentence("foo", Seq(variable)))),
      Rule(AtomicSentence("terminal"), Seq(AtomicSentence("true", Seq(constant))))
    ))

    flatten(description).rules.size == 0
  }

  property("substitute_multiple_inits") = forAll(Gen.nonEmptyListOf(inits(constants)), variables) { (inits, variable) =>
    val description = Description(inits ++ Seq(
      Rule(AtomicSentence("terminal"), Seq(AtomicSentence("true", Seq(variable))))
    ))

    flatten(description).rules.size == 2 * inits.toSet.size
  }

  property("introduce_object_constants") = forAll(Gen.nonEmptyListOf(inits(constants)), constants, variables) { (inits, constant, variable) =>
    val description = Description(inits ++ Seq(
      Rule(AtomicSentence("next", Seq(constant)), Seq(AtomicSentence("true", Seq(variable))))
    ))

    !inits.flatMap(_.head.terms).contains(constant) ==> (flatten(description).rules.size == 2 * inits.toSet.size + 1)
  }

  property("multiply_object_constants") = forAll(Gen.nonEmptyListOf(inits(constants)), functions(variables)) { (inits, function) =>
    val variables = function.collectVariables.toSeq
    val description = Description(inits ++ Seq(
      Rule(AtomicSentence("next", Seq(function)), variables.map(v => AtomicSentence("true", Seq(v))))
    ))

    val initsSetSize = inits.toSet.size
    flatten(description).rules.size == initsSetSize + Math.pow(initsSetSize, variables.size)
  }

  property("match_object_constants") = forAll(constants, variables, variables, variables) { (constant, var1, var2, var3) =>
    val description = Description(Seq(
      Rule(AtomicSentence("init", Seq(constant)), Seq()),
      Rule(AtomicSentence("legal", Seq(var1, var2)), Seq(AtomicSentence("true", Seq(var1)), AtomicSentence("true", Seq(var2)))),
      Rule(AtomicSentence("next", Seq(var3)), Seq(AtomicSentence("does", Seq(constant, var3))))
    ))

    flatten(description).rules.size == 3
  }

}
