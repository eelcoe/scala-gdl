package gdl.validation.dependencyGraph

import java.util.UUID

import gdl.lang._
import gremlin.scala.{id, label}
import org.apache.commons.lang.SerializationUtils


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
