package gdl.validation

import gdl.lang._
import gdl.validation.dependencyGraph._
import gremlin.scala._
import org.apache.commons.configuration.BaseConfiguration
import org.apache.tinkerpop.gremlin.process.traversal.Path
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph

import scala.collection.JavaConverters._

class DependencyGraph(val description: Description) {

  private val graph = {
    val graphBuilder = new GraphBuilder(TinkerGraph.open(DependencyGraph.conf).asScala)

    for (rule <- description.rules) graphBuilder.addRule(rule)

    graphBuilder.graph
  }

  def relations: Set[Relation] = graph.V.hasLabel[Relation].toCC[Relation].toSet
  def functions: Set[Function] = graph.V.hasLabel[Function].toCC[Function].toSet
  def constants: Set[ConstantArg] = graph.V.hasLabel[ConstantArg].toCC[ConstantArg].toSet
  def variables: Set[VariableArg] = graph.V.hasLabel[VariableArg].toCC[VariableArg].toSet

  lazy val cycles: Set[List[(Relation, InRuleFor, Relation)]] = {
    for {
      relation <- graph.V
      path <- relation.start.emit.repeat(_.outE.inV.simplePath).outE.inV.where(_.is(relation)).path
    } yield collectEdges(path)
  }.toSet

  def paths(from: Relation, to: Relation): Set[List[(Relation, InRuleFor, Relation)]] = {
    for {
      origin <- graph.V(from.id)
      destination <- graph.V(to.id)
      path <- origin.start.emit.repeat(_.outE.inV.simplePath).outE.inV.where(_.is(destination)).path
    } yield collectEdges(path)
  }.toSet

  def strata: Map[Int, Set[Relation]] = {
    for {
      relation <- graph.V.hasLabel[Relation]
      path <- relation.start.emit.repeat(_.inE.outV.simplePath).path
    } yield (relation.toCC[Relation], countNegatives(path))
  }.toList
    .groupBy { case (relation, count) => relation }
    .mapValues(_.map { case (_, count) => count }.max)
    .groupBy { case (_, maxCount) => maxCount }
    .mapValues(_.keySet)

  private def collectEdges(path: Path): List[(Relation, InRuleFor, Relation)] = path.objects.asScala.collect {
    case e: Edge => (e.outVertex.toCC[Relation], e.toCC[InRuleFor], e.inVertex.toCC[Relation])
  }.toList

  private def countNegatives(path: Path): Int = collectEdges(path).count { case (_, InRuleFor(negative), _) => negative }
}

object DependencyGraph {

  def conf = {
    val configuration = new BaseConfiguration
    configuration.setProperty(TinkerGraph.GREMLIN_TINKERGRAPH_VERTEX_ID_MANAGER, TinkerGraph.DefaultIdManager.UUID.name)
    configuration.setProperty(TinkerGraph.GREMLIN_TINKERGRAPH_EDGE_ID_MANAGER, TinkerGraph.DefaultIdManager.INTEGER.name)
    configuration.setProperty(TinkerGraph.GREMLIN_TINKERGRAPH_VERTEX_PROPERTY_ID_MANAGER, TinkerGraph.DefaultIdManager.LONG.name)
    configuration
  }
}
