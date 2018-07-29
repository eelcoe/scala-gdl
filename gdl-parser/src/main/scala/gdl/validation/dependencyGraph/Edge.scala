package gdl.validation.dependencyGraph

import gremlin.scala.label

@label("inRuleFor")
case class InRuleFor(negative: Boolean)

@label("argumentOf")
case class ArgumentOf(position: Int)
