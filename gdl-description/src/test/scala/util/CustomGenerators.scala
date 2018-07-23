package util

import org.scalacheck.Gen

import scala.collection.convert.ImplicitConversions._

object CustomGenerators {
  def nonEmptyListOfMax[T](i: Int, elemGen: Gen[T]) = for {
    n <- Gen.choose(1, i)
    list <- Gen.listOfN(n, elemGen)
  } yield list

  def nonNegNum = Gen.sized(n => Gen.choose(0, n))

  // generates a list of n subsets so that the total union is equal to the original set, or the empty set if n = 0
  def listOfNSubsets[T](n: Int, set: Set[T]): Gen[List[Set[T]]] =
    if (n == 0 || set.isEmpty) Gen.const(List.fill(n)(Set.empty))
    else {
      val allocationsGens = set.map(elem => for (i <- Gen.choose(1, n)) yield (i, elem))
      for {
        allocations <- Gen.sequence(allocationsGens)
      } yield {
        val subsets = asMapToSet(allocations)
        (1 to n).map(i => subsets.getOrElse(i, Set.empty))
      }.toList
    }

  private def asMapToSet[K, V](seq: Seq[(K, V)]): Map[K, Set[V]] = seq.groupBy(_._1).mapValues(_.map(_._2).toSet)
}
