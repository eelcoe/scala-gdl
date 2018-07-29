package gdl.validation

import gdl.lang._
import gdl.validation.dependencyGraph.{Function, InRuleFor, Relation}

class Restrictions(description: Description) {
  val dependencyGraph = new DependencyGraph(description)

  def validateUniqueNames: Set[String] = validateUniqueRelations union validateUniqueFunctions
  private def validateUniqueRelations: Set[String] =
    dependencyGraph.relations.groupBy(_.name).collect(nonUniqueRelationMessage).toSet
  private def validateUniqueFunctions: Set[String] =
    dependencyGraph.functions.groupBy(_.name).collect(nonUniqueFunctionMessage).toSet

  def validateStratification: Set[String] = dependencyGraph.cycles.filter(hasNegativePart).map(stratificationMessage)

  def validateKeywords: Set[String] = validateDoesKeyword union validateInitKeyword
  def validateDoesKeyword: Set[String] = {
    paths(Does, Goal) union paths(Does, Init) union paths(Does, Legal) union paths(Does, Terminal)
  }.map(forbiddenPathMessage)

  def validateInitKeyword: Set[String] = {
    paths(Goal, Init) union paths(Legal, Init) union paths(Terminal, Init) union paths(True, Init)
  }.map(forbiddenPathMessage)

  private def paths(from: RelationConstant, to: RelationConstant): Set[List[(Relation, InRuleFor, Relation)]] = for {
    origin <- dependencyGraph.relations.find(_.name == from.name).toSet[Relation]
    destination <- dependencyGraph.relations.find(_.name == to.name).toSet[Relation]
    result <- dependencyGraph.paths(origin, destination)
  } yield result

  def validateRecursion: Set[String] = {
    for {
      rule <- description.rules
      headRelation = rule.head.relation
      literal <- rule.body
      if dependencyGraph.cycles.exists(pathContains(headRelation, literal.relation))
      term <- terms(literal)
      if !term.isGround && !rule.head.terms.contains(term) && !rule.body.exists { other =>
        terms(other).contains(term) && !dependencyGraph.cycles.exists(pathContains(literal.relation, other.relation))
      }
    } yield s"rule for relation $headRelation violates recursion restriction for term $term in literal $literal: $rule"
  }.toSet

  def stratify: Either[Set[String], Map[Int, Set[Rule]]] = {
    val validationMessages = validateUniqueNames union validateStratification union validateKeywords union validateRecursion
    if (validationMessages.nonEmpty) Left(validationMessages)
    else Right(dependencyGraph.strata.mapValues(_.map(_.toRelationConstant)flatMap(getRulesByHeadRelation)))
  }

  private def getRulesByHeadRelation(relation: RelationConstant) = description.rules.filter(_.head.relation == relation)

  private def pathContains(relations: RelationConstant*) = { path: List[(Relation, InRuleFor, Relation)]  =>
    relations.toSet subsetOf path.map(_._1.toRelationConstant).toSet
  }

  private def terms(literal: Literal) = literal match {
    case AtomicSentence(_, terms) => terms
    case Not(AtomicSentence(_, terms)) => terms
    case Distinct(x, y) => Seq(x, y)
  }

  private val nonUniqueRelationMessage: PartialFunction[(String, Set[Relation]), String] = {
    case (name, relations) if relations.size > 1 =>
      s"relation '$name' occurs with different arities: ${relations.map(_.arity).toList.sorted.mkString(", ")}"
  }

  private val nonUniqueFunctionMessage: PartialFunction[(String, Set[Function]), String] = {
    case (name, relations) if relations.size > 1 =>
      s"function '$name' occurs with different arities: ${relations.map(_.arity).toList.sorted.mkString(", ")}"
  }

  private val hasNegativePart = (path: List[(Relation, InRuleFor, Relation)]) =>
    path.exists { case (_, InRuleFor(negative), _) => negative }

  private val stratificationMessage = (path: List[(Relation, InRuleFor, Relation)]) =>
    s"cycle for relation '${path.head._1.name}' violates stratification: ${pathToString(path)}"

  private def forbiddenPathMessage = (path: List[(Relation, InRuleFor, Relation)]) =>
    s"paths between '${path.head._1.name}' and '${path.last._3.name} are not allowed: ${pathToString(path)}"

  private def pathToString(path: List[(Relation, InRuleFor, Relation)]) = {
    path.flatMap { segment =>
      List(segment._1.toRelationConstant.toString, if(segment._2.negative) "~>" else "->")
    } :+ path.last._3.toRelationConstant.toString
  }.mkString(" ")
}
