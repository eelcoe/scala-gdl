package util

import org.scalacheck.Gen

object NameGenerators {
  def constants = Gen.oneOf(Gen.identifier, Gen.choose(0, 100).map(_.toString))

  def functions[T](argGen: Gen[T]) = for {
    name <- Gen.identifier.label("function_name")
    n <- Gen.choose(1, 3)
    args <- Gen.listOfN(n, argGen).label("function_arguments")
  } yield (name, args)

  def nestedFunctions = functions(functions(constants))

  def rules[T, U](headGen: Gen[T], bodyGen: Gen[U]) = for {
    head <- headGen.label("head")
    n <- Gen.choose(1, 3)
    body <- Gen.listOfN(n, bodyGen).label("body")
  } yield (head, body)

}
