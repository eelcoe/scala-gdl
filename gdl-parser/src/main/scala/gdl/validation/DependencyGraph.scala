package gdl.validation

import java.util.UUID

import gdl.lang._
import gremlin.scala._
import org.apache.commons.configuration.BaseConfiguration
import org.apache.commons.lang.SerializationUtils
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

  private def collectEdges(path: Path): List[(Relation, InRuleFor, Relation)] = path.objects.asScala.collect {
    case e: Edge => (e.outVertex.toCC[Relation], e.toCC[InRuleFor], e.inVertex.toCC[Relation])
  }.toList

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

class GraphBuilder(val graph: ScalaGraph) {

  def addRule(rule: Rule): Vertex = {
    val headRelation = buildNode(rule.head)

    for (literal <- rule.body) {
      val bodyRelation = buildNode(literal)
      literal match {
        case _: AtomicSentence => bodyRelation --- InRuleFor(false) --> headRelation
        case _: Not => bodyRelation --- InRuleFor(true) --> headRelation
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

trait WithNameUuid {
  protected def createUUIDNotForCaseClassExtensions(source: Serializable) = UUID.nameUUIDFromBytes(SerializationUtils.serialize(source))
  protected def createUUID(source: Object) = UUID.nameUUIDFromBytes(source.toString.getBytes)
}

@label("relation")
case class Relation(@id id: UUID, name: String, arity: Int) {
  def toRelationConstant: RelationConstant = RelationConstant(name, arity)
}

object Relation extends WithNameUuid {
  def from(relation: RelationConstant) = Relation(createUUID(relation), relation.name, relation.arity)
}

@label("constant")
case class ConstantArg(@id id: UUID, name: String) {
  def toObjectConstant: ObjectConstant = ObjectConstant(name)
}

object ConstantArg extends WithNameUuid {
  def from(constant: ObjectConstant) = ConstantArg(createUUID(constant), constant.name)
}

@label("variable")
case class VariableArg(@id id: UUID, name: String) {
  def toVariable: Variable = Variable(name)
}

object VariableArg extends WithNameUuid {
  def from(variable: Variable) = VariableArg(createUUID(variable), variable.name)
}

@label("function")
case class Function(@id id: UUID, name: String, arity: Int) {
  def toFunctionConstant: FunctionConstant = FunctionConstant(name, arity)
}

object Function extends WithNameUuid {
  def from(function: FunctionConstant) = Function(createUUID(function), function.name, function.arity)
}

@label("inRuleFor")
case class InRuleFor(negative: Boolean)

@label("argumentOf")
case class ArgumentOf(position: Int)
