package util

import org.scalacheck.Gen

object CustomGenerators {
  def nonEmptyListOfMax[T](i: Int, elemGen: Gen[T]) = for {
    n <- Gen.choose(1, i)
    list <- Gen.listOfN(n, elemGen)
  } yield list
}
