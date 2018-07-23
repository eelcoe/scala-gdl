package gdl.propnet.node

trait Node {
  def get(): Boolean
}

abstract class ViewNode extends Node {
  private var in: Node = False
  def setIn(node: Node): Unit = in = node
  def get(): Boolean = in.get()
}

class And(in: Node*) extends Node {
  def get(): Boolean = in.forall(_.get())
}

class Or(in: Node*) extends Node {
  def get(): Boolean = in.exists(_.get())
}

class Not(in: Node) extends Node {
  def get(): Boolean = !in.get()
}

object True extends Node {
  def get(): Boolean = true
}

object False extends Node {
  def get(): Boolean = false
}

object Node {
  def and(nodes: Seq[Node]): Node =
    if (nodes.contains(False)) False
    else nodes.filterNot(_ == True) match {
      case Seq() => True
      case Seq(node) => node
      case statefulNodes => new And(statefulNodes: _*)
    }

  def or(nodes: Seq[Node]): Node =
    if (nodes.contains(True)) True
    else nodes.filterNot(_ == False) match {
      case Seq() => False
      case Seq(node) => node
      case statefulNodes => new Or(statefulNodes: _*)
    }

  def not(node: Node): Node = node match {
    case True => False
    case False => True
    case statefulNode => new Not(statefulNode)
  }
}