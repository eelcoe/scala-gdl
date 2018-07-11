package util

import gdl.lang._
import org.scalacheck.Gen

import scala.collection.convert.ImplicitConversions._

object ObjectGenerators {
  def constants = NameGenerators.constants.map(ObjectConstant.apply)

  def variables = Gen.identifier.map(Variable.apply)

  def functions(argGen: Gen[Term]) = for {
    name <- Gen.identifier.label("function_name")
    n <- Gen.choose(1, 3)
    args <- Gen.listOfN(n, argGen).label("function_arguments")
  } yield AppliedFunction(name, args)

  def nestedFunctions = functions(functions(constants))

  def atomicSentences(argGen: Gen[Term]) = for {
    name <- Gen.identifier.label("relation_name")
    n <- Gen.choose(0, 3)
    args <- Gen.listOfN(n, argGen).label("relation_arguments")
  } yield AtomicSentence(name, args)

  def rules(headGen: Gen[AtomicSentence], bodyGen: Gen[Literal]) = for {
    head <- headGen.label("rule_head")
    n <- Gen.choose(0, 3)
    body <- Gen.listOfN(n, bodyGen).label("rule_body")
  } yield Rule(head, body)

  def safeRules = for {
    numberOfVars <- Gen.choose(0, 3)
    vars <- Gen.listOfN(numberOfVars, variables).map(_.toSet)
    headVars <- subset(vars)
    negativeVars <- subset(vars)
    head <- atomicSentence(headVars)
    posLiterals <- listOfAtomicSentences(vars)
    negLiterals <- listOfAtomicSentences(negativeVars).map(_.map(Not.apply))
  } yield Rule(head, posLiterals ++ negLiterals)

  private def subset[T](set: Set[T]): Gen[Set[T]] = for {
    n <- Gen.choose(0, set.size)
    s <- Gen.pick(n, set)
  } yield s.toSet

  private def atomicSentence(variables: Set[Variable]): Gen[AtomicSentence] = for {
    name <- Gen.identifier.label("relation_name")
    n <- Gen.choose(if (variables.isEmpty) 0 else 1, 3)
    vars <- listOfNSubsets(n, variables)
    args <- Gen.sequence[List[Term], Term](vars.map(termsContaining(_))).label("relation_arguments")
  } yield AtomicSentence(name, args)

  private def listOfAtomicSentences(variables: Set[Variable]): Gen[List[AtomicSentence]] = for {
    n <- Gen.choose(if (variables.isEmpty) 0 else 1, 3)
    vars <- listOfNSubsets(n, variables)
    sentences <- Gen.sequence[List[AtomicSentence], AtomicSentence](vars.map(atomicSentence))
  } yield sentences

  def termsContaining(variables: Set[Variable], recursionLevel: Int = 0): Gen[Term] = {
    val varsGen = for {
      n <- Gen.choose(1, 3)
      vars <- listOfNSubsets(n, variables)
    } yield vars

    variables.size match {
      case 0 => constantTerm(recursionLevel)
      case 1 => variableTerm(variables.toSeq.head, varsGen, recursionLevel)
      case _ => functionTerm(variables.toList.map(Set(_)), varsGen, recursionLevel)
    }
  }

  private def constantTerm(recursionLevel: Int): Gen[Term] =
    if (recursionLevel < 2) Gen.frequency((4, constants), (1, functions(constants))) else constants

  private def variableTerm(variable: Variable, varsGen: Gen[List[Set[Variable]]], recursionLevel: Int): Gen[Term] =
    if (recursionLevel < 2) Gen.frequency((4, Gen.const(variable)), (1, function(varsGen, recursionLevel))) else Gen.const(variable)

  private def functionTerm(variableSets: List[Set[Variable]], varsGen: Gen[List[Set[Variable]]], recursionLevel: Int): Gen[Term] =
    if (recursionLevel < 2) function(varsGen, recursionLevel) else function(Gen.const(variableSets), recursionLevel)

  private def function(varsGen: Gen[List[Set[Variable]]], recursionLevel: Int): Gen[AppliedFunction] = for {
    name <- Gen.identifier.label("function_name")
    vars <- varsGen
    args <- Gen.sequence[List[Term], Term](vars.map(termsContaining(_, recursionLevel + 1))).label("function_arguments")
  } yield AppliedFunction(name, args)

  // generates a list of n subsets so that the total union is equal to the original set, or the empty set if n = 0
  private def listOfNSubsets[T](n: Int, set: Set[T]): Gen[List[Set[T]]] =
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
