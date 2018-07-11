package gdl.validation

import gdl.lang._
import org.scalatest.WordSpec
import org.scalatest.Matchers._

class RestrictionsTest extends WordSpec {

  val foo = AtomicSentence("foo")
  val bar = ObjectConstant("bar")
  val fooBar = AtomicSentence("foo", Seq(bar))

  val fun1 = AppliedFunction("f", Seq(Variable("x")))
  val fun2 = AppliedFunction("f", Seq(Variable("x"), Variable("y")))

  "Restrictions" should {
    "permit function constants with the same name and arity" in {
      val restrictions = new Restrictions(Description(Seq(
        Rule(foo, Seq(AtomicSentence("true", Seq(fun1)))),
        Rule(foo, Seq(AtomicSentence("true", Seq(fun1))))
      )))
      restrictions.validateUniqueNames should have size 0
    }
    "permit relation constants with the same name and arity" in {
      val restrictions = new Restrictions(Description(Seq(Rule(foo, Seq()), Rule(foo, Seq()))))
      restrictions.validateUniqueNames should have size 0
    }
    "permit cycles without negative literals" in {
      val restrictions = new Restrictions(Description(Seq(Rule(foo, Seq(foo)))))
      restrictions.validateStratification should have size 0
    }
    "permit correct usage of does" in {
      val restrictions = new Restrictions(Description(Seq(
        Rule(Next(bar), Seq(Does(ObjectConstant("player"), fun1)))
      )))
      restrictions.validateDoesKeyword should have size 0
    }
    "permit correct usage of init" in {
      val restrictions = new Restrictions(Description(Seq(
        Rule(Init(fun1), Seq(Role(fun1)))
      )))
      restrictions.validateInitKeyword should have size 0
    }
    "permit cycles for relations with deterministic free variables" in {
      val restrictions = new Restrictions(Description(Seq(
        Rule(fooBar, Seq(AtomicSentence("foo", Seq(Variable("x"))), AtomicSentence("bar", Seq(Variable("x")))))
      )))
      println(restrictions.validateRecursion)
      restrictions.validateRecursion should have size 0
    }
    "forbid function constants with the same name but different arity" in {
      val restrictions = new Restrictions(Description(Seq(
        Rule(foo, Seq(AtomicSentence("true", Seq(fun1)))),
        Rule(foo, Seq(AtomicSentence("true", Seq(fun2))))
      )))
      restrictions.validateUniqueNames should have size 1
    }
    "forbid relation constants with the same name but different arity" in {
      val restrictions = new Restrictions(Description(Seq(Rule(foo, Seq()), Rule(fooBar, Seq()))))
      restrictions.validateUniqueNames should have size 1
    }
    "forbid cycles with negative literals" in {
      val restrictions = new Restrictions(Description(Seq(Rule(foo, Seq(Not(foo))))))
      restrictions.validateStratification should have size 1
    }
    "forbid incorrect usage of does" in {
      val restrictions = new Restrictions(Description(Seq(
        Rule(Goal(ObjectConstant("player"), ObjectConstant("0")), Seq(Does(ObjectConstant("player"), fun1)))
      )))
      restrictions.validateDoesKeyword should have size 1
    }
    "forbid incorrect usage of init" in {
      val restrictions = new Restrictions(Description(Seq(
        Rule(Init(bar), Seq(foo)),
        Rule(foo, Seq(Legal(ObjectConstant("player"), bar)))
      )))
      restrictions.validateInitKeyword should have size 1
    }
    "forbid cycles for relations with undeterministic free variables" in {
      val restrictions = new Restrictions(Description(Seq(
        Rule(fooBar, Seq(AtomicSentence("foo", Seq(Variable("x")))))
      )))
      restrictions.validateRecursion should have size 1
    }
  }
}
