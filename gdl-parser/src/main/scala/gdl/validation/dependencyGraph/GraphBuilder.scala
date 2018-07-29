package gdl.validation.dependencyGraph

import gdl.lang._
import gremlin.scala._

class GraphBuilder(val graph: ScalaGraph) {

  def addRule(rule: Rule): Vertex = {
    val headRelation = buildNode(rule.head)

    for (literal <- rule.body) {
      val bodyRelation = buildNode(literal)
      literal match {
        case _: AtomicSentence => bodyRelation --- InRuleFor(false) --> headRelation
        case _: Not => bodyRelation --- InRuleFor(true) --> headRelation
        case _: Distinct => bodyRelation --- InRuleFor(true) --> headRelation
      }
    }

    headRelation
  }

  def buildNode(literal: Literal): Vertex = {
    literal match {
      case AtomicSentence(relationConstant, terms) => {
        val relation = getOrCreateVertex(graph, Relation.from(relationConstant))

        for (index <- terms.indices) {
          val argument = buildNode(terms(index))
          argument --- ArgumentOf(index + 1) --> relation
        }

        relation
      }
      case Not(atomicSentence) => buildNode(atomicSentence)
      case Distinct(x, y) => buildNode(AtomicSentence(Distinct, Seq(x, y)))
    }
  }

  def buildNode(term: Term): Vertex = {
    term match {
      case objectConstant: ObjectConstant => getOrCreateVertex(graph, ConstantArg.from(objectConstant))
      case variable: Variable => getOrCreateVertex(graph, VariableArg.from(variable))
      case AppliedFunction(funcionConstant, terms) => {
        val function = getOrCreateVertex(graph, Function.from(funcionConstant))

        for (index <- terms.indices) {
          val argument = buildNode(terms(index))
          argument --- ArgumentOf(index + 1) --> function
        }

        function
      }
    }
  }

  private def getOrCreateVertex(graph: ScalaGraph, relation: Relation) =
    graph.V(relation.id).toList.headOption.getOrElse(graph.addVertex(relation))

  private def getOrCreateVertex(graph: ScalaGraph, constant: ConstantArg) =
    graph.V(constant.id).toList.headOption.getOrElse(graph.addVertex(constant))

  private def getOrCreateVertex(graph: ScalaGraph, variable: VariableArg) =
    graph.V(variable.id).toList.headOption.getOrElse(graph.addVertex(variable))

  private def getOrCreateVertex(graph: ScalaGraph, function: Function) =
    graph.V(function.id).toList.headOption.getOrElse(graph.addVertex(function))
}
