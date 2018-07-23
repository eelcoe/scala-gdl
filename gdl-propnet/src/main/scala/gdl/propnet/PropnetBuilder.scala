package gdl.propnet

import gdl.lang.{AtomicSentence, Description, Literal, Rule}
import gdl.propnet.node._

import scala.collection.mutable

class PropnetBuilder() {
  private val inputs = mutable.HashSet[Input]()
  private val states = mutable.HashSet[State]()
  private val views = mutable.HashSet[View]()
  private val goals = mutable.HashSet[Goal]()
  private val roles = mutable.HashSet[Role]()
  private val terminal: Terminal = Terminal()

  def createNodes(model: Set[AtomicSentence]): Unit = model.foreach {
    case AtomicSentence(gdl.lang.Does, Seq(player, move)) => inputs.add(Input(Player(player), Move(move)))
    case AtomicSentence(gdl.lang.Legal, _) => ()
    case AtomicSentence(gdl.lang.Init, _) => ()
    case AtomicSentence(gdl.lang.True, Seq(term)) => states.add(State(term))
    case AtomicSentence(gdl.lang.Next, _) => ()
    case AtomicSentence(gdl.lang.Goal, Seq(player, score)) => goals.add(Goal(Player(player), Score(score)))
    case AtomicSentence(gdl.lang.Terminal, _) => ()
    case AtomicSentence(gdl.lang.Role, Seq(player)) => roles.add(Role(Player(player)))
    case sentence => views.add(View(sentence))
  }

  def connectNodes(flattenedRules: Seq[Rule]): Unit = for ((head, rules) <- flattenedRules.groupBy(_.head)) {
    findViewNode(head).foreach(_.setIn(Node.or(rules.map(rule => Node.and(rule.body.map(literal => createNode(literal)))))))
  }

  def build(): Propnet = {
    new Propnet(inputs.toSet, states.toSet, views.toSet, goals.toSet, roles.toSet, terminal)
  }

  private def findViewNode(sentence: AtomicSentence): Option[ViewNode] = sentence match {
    case AtomicSentence(gdl.lang.Legal, Seq(player, move)) => inputs.find(_ == Input(Player(player), Move(move))).map(_.legal)
    case AtomicSentence(gdl.lang.Init, Seq(term)) => states.find(_ == State(term)).map(_.init)
    case AtomicSentence(gdl.lang.Next, Seq(term)) => states.find(_ == State(term)).map(_.next)
    case AtomicSentence(gdl.lang.Goal, Seq(player, score)) => goals.find(_ == Goal(Player(player), Score(score)))
    case AtomicSentence(gdl.lang.Terminal, Seq()) => Some(terminal)
    case AtomicSentence(gdl.lang.Role, Seq(player)) => roles.find(_ == Role(Player(player)))
    case _ => views.find(_ == View(sentence))
  }

  private def createNode(literal: Literal): Node = literal match {
    case sentence: AtomicSentence => findNode(sentence).getOrElse(False)
    case gdl.lang.Not(sentence) => Node.not(createNode(sentence))
    case gdl.lang.Distinct(x, y) => if (x == y) True else False
  }

  private def findNode(sentence: AtomicSentence): Option[Node] = sentence match {
    case AtomicSentence(gdl.lang.Does, Seq(player, move)) => inputs.find(_ == Input(Player(player), Move(move)))
    case AtomicSentence(gdl.lang.True, Seq(term)) => states.find(_ == State(term))
    case _ => findViewNode(sentence)
  }
}

object PropnetBuilder {
  def create(description: Description): Propnet = {
    val modelBuilder = new ModelBuilder(description)

    val model = modelBuilder.build()
    val flattenedRules = description.rules.flatMap(modelBuilder.applyInstances)

    val propnetBuilder = new PropnetBuilder()
    propnetBuilder.createNodes(model)
    propnetBuilder.connectNodes(flattenedRules)

    propnetBuilder.build()
  }
}
