package gdl.propnet.node

import gdl.lang.Term

case class State(term: Term) extends Node {
  private var currentState: Boolean = false
  def set(state: Boolean): Unit = currentState = state
  def get(): Boolean = currentState

  val next: Next = Next(term)
  def evaluate(): Unit = next.evaluate()
  def update(): Unit = set(next.getNext())

  val init: Init = Init(term)
  def initialize(): Unit = set(init.get())
}

case class Init(term: Term) extends ViewNode
case class Next(term: Term) extends ViewNode {
  private var nextState: Boolean = false
  def evaluate(): Unit = nextState = get()
  def getNext(): Boolean = nextState
}
