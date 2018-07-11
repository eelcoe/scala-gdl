package gdl.validation

import gdl.lang._
import org.scalatest.WordSpec
import org.scalatest.Matchers._

class DependencyGraphTest extends WordSpec {

  val foo = AtomicSentence("foo")
  val bar = AtomicSentence("bar")
  val fun = AppliedFunction("fun", Seq(ObjectConstant("a")))

  "dependency graph" should {
    "de-duplicate relations" in {
      val graph = new DependencyGraph(Description(Seq(Rule(foo, Seq()), Rule(foo, Seq()))))
      graph.relations should have size 1
    }
    "de-duplicate functions" in {
      val graph = new DependencyGraph(Description(Seq(Rule(AtomicSentence("foo", Seq(fun, fun)), Seq()))))
      graph.relations should have size 1
    }
    "detect cycles of length 1" in {
      val graph = new DependencyGraph(Description(Seq(Rule(foo, Seq(foo)))))
      graph.cycles should have size 1
      graph.cycles.map(_.size) should equal (Set(1))
    }
    "detect cycles of length 2" in {
      val graph = new DependencyGraph(Description(Seq(Rule(foo, Seq(bar)), Rule(bar, Seq(foo)))))
      graph.cycles should have size 2
      graph.cycles.map(_.size) should equal (Set(2))
    }
  }
}
