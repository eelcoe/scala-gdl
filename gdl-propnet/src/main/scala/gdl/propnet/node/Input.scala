package gdl.propnet.node

import gdl.lang.Term

case class Player(term: Term)
case class Move(term: Term)
case class Score(term: Term)

case class Input(player: Player, move: Move) extends Node {
  private var currentState: Boolean = false
  def set(state: Boolean): Unit = currentState = state
  def get(): Boolean = currentState

  val legal: Legal = Legal(player, move)
  def isLegal(): Boolean = legal.get()
}

case class Legal(player: Player, move: Move) extends ViewNode
case class Goal(player: Player, score: Score) extends ViewNode
case class Role(player: Player) extends ViewNode
