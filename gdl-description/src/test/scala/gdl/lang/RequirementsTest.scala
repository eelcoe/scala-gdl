package gdl.lang

import org.scalacheck.Prop.{BooleanOperators, forAll, throws}
import org.scalacheck.{Gen, Properties}
import util.ObjectGenerators

class RequirementsTest extends Properties("Valid_GDL") {
  import ObjectGenerators._

  val requirementViolated = throws(classOf[IllegalArgumentException])(_)

  property("init_in_head_permitted") = forAll(functions(constants)) { function: AppliedFunction =>
    Rule(AtomicSentence("init", Seq(function)), Seq())
    true
  }
  property("next_in_head_permitted") = forAll(functions(constants)) { function: AppliedFunction =>
    Rule(AtomicSentence("next", Seq(function)), Seq())
    true
  }
  property("distinct_in_body_permitted") = forAll(constants, constants) { (constant1: ObjectConstant, constant2: ObjectConstant) =>
    Rule(AtomicSentence("foo"), Seq(Distinct(constant1, constant2)))
    true
  }
  property("does_in_body_permitted") = forAll(functions(constants)) { function: AppliedFunction =>
    Rule(AtomicSentence("foo"), Seq(AtomicSentence("does", Seq(ObjectConstant("player"), function))))
    true
  }
  property("true_in_body_permitted") = forAll(functions(constants)) { function: AppliedFunction =>
    Rule(AtomicSentence("foo"), Seq(AtomicSentence("does", Seq(function))))
    true
  }
  property("role_in_head_permitted_for_empty_body") = forAll(functions(constants)) { function: AppliedFunction =>
    Rule(AtomicSentence("role", Seq(function)), Seq())
    true
  }
  property("safety_for_zero_variables") = forAll(Gen.nonEmptyListOf(atomicSentences(constants))) { case head :: body =>
    Rule(head, body)
    true
  }
  property("safety_for_identity") = forAll(atomicSentences(variables)) { atomicSentence: AtomicSentence =>
    Rule(atomicSentence, Seq(atomicSentence))
    true
  }
  property("safety_for_identity_function") = forAll(atomicSentences(functions(variables))) { atomicSentence: AtomicSentence =>
    Rule(atomicSentence, Seq(atomicSentence))
    true
  }
  property("safety_for_pos_and_neg_literals") = forAll(atomicSentences(variables)) { atomicSentence: AtomicSentence =>
    Rule(AtomicSentence("foo"), Seq(atomicSentence, Not(atomicSentence)))
    true
  }
  property("safety_for_distinct_literals") = forAll(constants, variables) { (constant: ObjectConstant, variable: Variable) =>
    Rule(AtomicSentence("foo"), Seq(AtomicSentence("bar", Seq(variable)), Distinct(constant, variable)))
    true
  }
  property("distinct_as_atomic_sentence_forbidden") = forAll(constants, constants) { (constant1: ObjectConstant, constant2: ObjectConstant) =>
    requirementViolated(AtomicSentence("distinct", Seq(constant1, constant2)))
  }
  property("init_in_body_forbidden") = forAll(functions(constants)) { function: AppliedFunction =>
    requirementViolated(Rule(AtomicSentence("foo"), Seq(AtomicSentence("init", Seq(function)))))
  }
  property("next_in_body_forbidden") = forAll(functions(constants)) { function: AppliedFunction =>
    requirementViolated(Rule(AtomicSentence("foo"), Seq(AtomicSentence("next", Seq(function)))))
  }
  property("does_in_head_forbidden") = forAll(functions(constants)) { function: AppliedFunction =>
    requirementViolated(Rule(AtomicSentence("does", Seq(ObjectConstant("player"), function)), Seq()))
  }
  property("true_in_head_forbidden") = forAll(functions(constants)) { function: AppliedFunction =>
    requirementViolated(Rule(AtomicSentence("true", Seq(function)), Seq()))
  }
  property("role_in_head_forbidden_for_nonempty_body") = forAll(functions(variables)) { function: AppliedFunction =>
    requirementViolated(Rule(AtomicSentence("role", Seq(function)), Seq(AtomicSentence("true", Seq(function)))))
  }
  property("safety_violated_by_head") = forAll(atomicSentences(variables)) { atomicSentence: AtomicSentence =>
    (!atomicSentence.isGround) ==> requirementViolated(Rule(atomicSentence, Seq()))
  }
  property("safety_violated_by_negation") = forAll(atomicSentences(variables)) { atomicSentence: AtomicSentence =>
    (!atomicSentence.isGround) ==> requirementViolated(Rule(AtomicSentence("foo"), Seq(Not(atomicSentence))))
  }
  property("safety_violated_by_head_and_negation") = forAll(atomicSentences(variables)) { atomicSentence: AtomicSentence =>
    (!atomicSentence.isGround) ==> requirementViolated(Rule(atomicSentence, Seq(Not(atomicSentence))))
  }
  property("safety_violated_by_function_in_head") = forAll(atomicSentences(functions(variables))) { atomicSentence: AtomicSentence =>
    (!atomicSentence.isGround) ==> requirementViolated(Rule(atomicSentence, Seq()))
  }
  property("safety_violated_by_distinct") = forAll(constants, variables) { (constant: ObjectConstant, variable: Variable) =>
    requirementViolated(Rule(AtomicSentence("foo"), Seq(Distinct(constant, variable))))
  }
}
