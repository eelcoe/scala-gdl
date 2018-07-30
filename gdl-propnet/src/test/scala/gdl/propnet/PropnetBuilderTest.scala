package gdl.propnet

import gdl.lang._
import gdl.propnet.node.{Move, Player, Score, State}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Gen, Prop, Properties}
import util.{CustomGenerators, ObjectGenerators}

class PropnetBuilderTest extends Properties("Build_propnet") {
  import CustomGenerators._
  import ObjectGenerators._

  def playerMoves: Gen[Map[Term, Set[Term]]] = for {
    players <- nonEmptyListOfMax[Term](10, constants)
    allMoves <- Gen.listOf[Term](constants)
    moves <- listOfNSubsets(players.size, allMoves.toSet)
  } yield (players zip moves).toMap

  def state(name: String, i: Int) = AppliedFunction(name, Seq(ObjectConstant(i.toString)))

  def build(model: AtomicSentence*)(rules: Rule*) = buildPropnet(model, rules)

  def buildPropnet(model: Seq[AtomicSentence], rules: Seq[Rule] = Seq()) = {
    val propnetBuilder = new PropnetBuilder()
    propnetBuilder.createNodes(model.toSet)
    propnetBuilder.connectNodes(rules)

    propnetBuilder.build()
  }

  property("players") = forAll(Gen.listOf(constants)) { players =>
    val propnet = buildPropnet(
      players.map(Role.apply)
    )

    propnet.players == players.map(Player).toSet
  }

  property("moves") = forAll(Gen.listOf(constants)) { moves =>
    val propnet = buildPropnet(
      Role(ObjectConstant("p")) +: moves.map(Does.apply(ObjectConstant("p"), _))
    )

    propnet.moves == moves.map(Move).toSet
  }

  property("scores") = forAll(Gen.listOf(constants)) { scores =>
    val propnet = buildPropnet(
      Role(ObjectConstant("p")) +: scores.map(Goal.apply(ObjectConstant("p"), _))
    )

    propnet.scores == scores.map(score => Score(score)).toSet
  }

  property("legal_moves") = forAll(playerMoves) { playerMoves =>
    val players = playerMoves.keys.toSeq
    val pairs = for {
      (player, moves) <- playerMoves.toSeq
      move <- moves
    } yield (player, move)

    val propnet = buildPropnet(
      players.map(player => Role(player)) ++ pairs.map { case (player, move) => Does(player, move) },
      pairs.map { case (player, move) => Rule(Legal(player, move), Seq()) }
    )

    propnet.players.forall(player => propnet.getLegalMoves(player) == playerMoves(player.term).map(Move))
  }

  property("initially_true") = forAll(nonNegNum) { n =>
    val propnet = build(
      True(ObjectConstant("c"))
    )(
        Rule(Init(ObjectConstant("c")), Seq())
      )

    propnet.init()
    for (_ <- 1 to n) propnet.update()

    propnet.states.size == 1 && propnet.states.forall(_.get() == (n == 0))
  }

  property("initially_false") = forAll(nonNegNum) { n =>
    val propnet = build(
      True(ObjectConstant("c"))
    )(
        Rule(Next(ObjectConstant("c")), Seq())
      )

    propnet.init()
    for (_ <- 1 to n) propnet.update()

    propnet.states.size == 1 && propnet.states.forall(_.get() == (n > 0))
  }

  property("always_true") = forAll(nonNegNum) { n =>
    val propnet = build(
      True(ObjectConstant("c"))
    )(
        Rule(Init(ObjectConstant("c")), Seq()),
        Rule(Next(ObjectConstant("c")), Seq(True(ObjectConstant("c"))))
      )

    propnet.init()
    for (_ <- 1 to n) propnet.update()

    propnet.states.size == 1 && propnet.states.forall(_.get())
  }

  property("always_false") = forAll(nonNegNum) { n =>
    val propnet = build(
      True(ObjectConstant("c"))
    )(
        Rule(Next(ObjectConstant("c")), Seq(True(ObjectConstant("c"))))
      )

    propnet.init()
    for (_ <- 1 to n) propnet.update()

    propnet.states.size == 1 && propnet.states.forall(!_.get())
  }

  property("introduce_view") = forAll(nonNegNum) { n =>
    val view = AtomicSentence("v", Seq())
    val propnet = build(
      True(ObjectConstant("c")),
      view
    )(
        Rule(Init(ObjectConstant("c")), Seq()),
        Rule(view, Seq(True(ObjectConstant("c")))),
        Rule(Next(ObjectConstant("c")), Seq(view))
      )

    propnet.init()
    for (_ <- 1 to n) propnet.update()

    propnet.states.size == 1 && propnet.views.size == 1 && propnet.states.forall(_.get())
  }

  property("terminal") = forAll(nonEmptyListOfMax(3, atomicSentences(constants))) { views =>
    val propnet = buildPropnet(views, views.tail.map(Rule(_, Seq())) ++ views.map(v => Rule(Terminal(), Seq(v))))

    propnet.views.size == views.size && propnet.terminal.get() == (views.size > 1)
  }

  property("period_m") = forAll(Gen.posNum[Int], nonNegNum) { (m, n) =>
    val model = for (i <- 0 until m) yield True(state("s", i))
    val rules = for (i <- 0 until m) yield Rule(Next(state("s", (i + 1) % m)), Seq(True(state("s", i))))

    val propnet = buildPropnet(model, Rule(Init(state("s", 0)), Seq()) +: rules)

    propnet.init()
    for (_ <- 1 to n) propnet.update()

    propnet.states.size == m && (0 until m).forall { i =>
      propnet.states.exists(s => s == State(state("s", i)) && s.get() == (i == n % m))
    }
  }

  property("period_multiplication") = forAll(Gen.posNum[Int], Gen.posNum[Int], nonNegNum) { (l, m, n) =>
    val model = (for (i <- 0 until l) yield True(state("s", i))) ++ (for (i <- 0 until m) yield True(state("t", i)))
    val rules = (for (i <- 0 until l) yield Rule(Next(state("s", (i + 1) % l)), Seq(True(state("s", i))))) ++
      (for (j <- 0 until m) yield Rule(Next(state("t", j)), Seq(Not(True(state("s", l - 1))), True(state("t", j))))) ++
      (for (j <- 0 until m) yield Rule(Next(state("t", (j + 1) % m)), Seq(True(state("s", l - 1)), True(state("t", j)))))

    val propnet = buildPropnet(model, Rule(Init(state("s", 0)), Seq()) +: Rule(Init(state("t", 0)), Seq()) +: rules)

    propnet.init()
    for (_ <- 1 to n) propnet.update()

    propnet.states.size == l + m && (0 until l).forall { i =>
      propnet.states.exists(s => s == State(state("s", i)) && s.get() == (i == n % l))
    } && (0 until m).forall { i =>
      propnet.states.exists(s => s == State(state("t", i)) && s.get() == (i == (n / l) % m))
    }
  }

  property("period_n_toggle") = forAll(Gen.posNum[Int], Gen.listOf(Gen.oneOf("noop", "toggle"))) { (n, moves) =>
    val player = ObjectConstant("p")
    val noop = ObjectConstant("noop")
    val toggle = ObjectConstant("toggle")

    val model = for (i <- 0 until n) yield True(state("s", i))
    val rules = (for (i <- 0 until n) yield Rule(Next(state("s", i)), Seq(Does(player, noop), True(state("s", i))))) ++
      (for (i <- 0 until n) yield Rule(Next(state("s", (i + 1) % n)), Seq(Does(player, toggle), True(state("s", i)))))

    val propnet = buildPropnet(
      Role(player) +: Does(player, noop) +: Does(player, toggle) +: model,
      Rule(Init(state("s", 0)), Seq()) +:
        Rule(Legal(player, noop), Seq()) +:
        Rule(Legal(player, toggle), Seq()) +: rules
    )

    propnet.init()
    for (move <- moves) {
      propnet.enterMoves(Map(Player(ObjectConstant("p")) -> Move(ObjectConstant(move))))
      propnet.update()
    }

    propnet.states.size == n && (0 until n).forall { i =>
      propnet.states.exists(s => s == State(state("s", i)) && s.get() == (i == moves.count(_ == "toggle") % n))
    }
  }
}
