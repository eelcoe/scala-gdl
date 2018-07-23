package gdl.propnet

import gdl.propnet.node._

class Propnet(val inputs: Set[Input], val states: Set[State], val views: Set[View], val goals: Set[Goal], val roles: Set[Role], val terminal: Terminal) {
  val players: Set[Player] = roles.map(_.player)
  val moves: Set[Move] = inputs.map(_.move)
  val scores: Set[Score] = goals.map(_.score)

  def init(): Unit = states.foreach(_.initialize())
  def update(): Unit = {
    states.foreach(_.evaluate())
    states.foreach(_.update())
  }

  def enterMoves(moves: Map[Player, Move]): Unit =
    inputs.foreach(input => input.set(moves.get(input.player).exists(_ == input.move)))

  def getLegalMoves(player: Player): Set[Move] =
    inputs.filter(input => input.player == player && input.legal.get()).map(_.move)
}
