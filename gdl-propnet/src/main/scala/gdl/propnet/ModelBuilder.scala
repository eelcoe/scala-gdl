package gdl.propnet

import gdl.lang._

import scala.annotation.tailrec
import scala.collection.mutable

/*
 * Builds a model that describes possible states in the game
 * All `init`, `next`, and `legal` literals are replaced by their counterparts `true` and `does`
 * Both of these may not appear in the heads of rules, whereas in turn `init` and `next` may only appear there
 * However, `legal` has no such restriction and thus behaves differently (whatever that means, haha)
 *
 * Returns a full model where `init`, `next`, and `legal` sentences are added for completeness
 */
class ModelBuilder(description: Description) {

  val groundSentences = new GroundSentences(description.rules.map(_.head))

  def build(): Set[AtomicSentence] = {

    val (facts, rules) = description.rules.partition(_.body.isEmpty)

    groundSentences.add(facts.map(_.head))

    var doNextIteration = true
    while (doNextIteration) {
      doNextIteration = false
      for (rule <- rules) {
        val newInstances = applyInstances(rule).map(_.head).filterNot(groundSentences.contains)
        if (newInstances.nonEmpty) {
          groundSentences.add(newInstances.toSet)
          doNextIteration = true
        }
      }
    }

    val denormalizedSentences = Set(Init, Next, Legal).flatMap { relation =>
      description.rules.filter(_.head.relation == relation).flatMap(applyInstances).map(_.head)
    }

    groundSentences.getValues ++ denormalizedSentences
  }

  def applyInstances(rule: Rule): Seq[Rule] = {
    import ModelBuilder.RuleDecorator

    val rulesOpt = rule.body
      .collect({ case sentence: AtomicSentence => (sentence.collectVariables.headOption, sentence) })
      .collectFirst({ case (Some(variable), sentence) =>
        val finder = new SubstitutionFinder(variable, sentence)
        for {
          instance <- groundSentences(sentence).toSeq
          constant <- finder(instance).toSeq
          rule <- applyInstances(rule.substitute(variable, constant))
        } yield rule
      })

    rulesOpt.getOrElse {
      if (rule.body.forall(isValid)) Seq(rule) else Seq()
    }
  }

  private def isValid(literal: Literal): Boolean = literal match {
    case sentence: AtomicSentence => groundSentences.contains(sentence)
    case _: Not => true
    case Distinct(x, y) => x != y
  }
}

/*
 * Finds a substitution for a specific variable in a sentence by checking an instance on compatibility
 * Returns the first object constant that has the same position in the instance as the variable in the sentence
 * Returns None if the rest of the instance cannot be matched to the rest of the sentence
 * Applies substitutions for all variables that are matched (not just the target) before looking at the rest
 */
class SubstitutionFinder(variable: Variable, sentence: AtomicSentence) {
  import ModelBuilder.TermDecorator

  final def apply(instance: AtomicSentence) =
    if (instance.relation == sentence.relation) matchPairs(sentence.terms.zip(instance.terms), None)
    else None

  @tailrec
  final def matchPairs(pairs: Seq[(Term, Term)], acc: Option[ObjectConstant]): Option[ObjectConstant] = pairs match {
    case Seq() => acc
    case (c1: ObjectConstant, c2: ObjectConstant) +: tail => if (c1 == c2) matchPairs(tail, acc) else None
    case (v: Variable, c: ObjectConstant) +: tail => matchPairs(substitute(v, c, tail), if (v == variable) Some(c) else None)
    case (f1: AppliedFunction, f2: AppliedFunction) +: tail => matchPairs(f1.terms.zip(f2.terms) ++ tail, acc)
    case _ => None
  }

  private def substitute(v: Variable, c: ObjectConstant, pairs: Seq[(Term, Term)]): Seq[(Term, Term)] =
    pairs.map({ case (x, y) => (x.substitute(v, c), y.substitute(v, c)) })
}

class GroundSentences(keys: Iterable[AtomicSentence]) {
  import ModelBuilder.AtomicSentenceDecorator

  private lazy val groundSentences = keys.map(sentence => (sentence.generalize, mutable.HashSet[AtomicSentence]())).toMap

  def add(sentence: AtomicSentence): Unit = groundSentences(sentence.generalize) += sentence.generalizeRelation

  def add(sentences: Iterable[AtomicSentence]): Unit =
    for ((key, values) <- sentences.groupBy(_.generalize)) groundSentences(key) ++= values.map(_.generalizeRelation)

  def apply(sentence: AtomicSentence): Set[AtomicSentence] =
    groundSentences.get(sentence.generalize).map(_.toSet).getOrElse(Set())

  def contains(sentence: AtomicSentence): Boolean =
    groundSentences.get(sentence.generalize).exists(_.contains(sentence.generalizeRelation))

  def getValues: Set[AtomicSentence] = groundSentences.values.map(_.toSet).reduceOption(_ union _).getOrElse(Set.empty)
}

object ModelBuilder {
  def flatten(description: Description): Description = {
    val builder = new ModelBuilder(description)
    builder.build()

    Description(description.rules.flatMap(builder.applyInstances))
  }

  implicit class RuleDecorator(val rule: Rule) extends AnyVal {
    def substitute(v: Variable, c: ObjectConstant): Rule =
      Rule(rule.head.substitute(v, c), rule.body.map(_.substitute(v, c)))
  }

  implicit class AtomicSentenceDecorator(val atomicSentence: AtomicSentence) extends AnyVal {
    def substitute(v: Variable, c: ObjectConstant): AtomicSentence =
      atomicSentence.copy(terms = atomicSentence.terms.map(_.substitute(v, c)))

    def generalize: AtomicSentence =
      AtomicSentence(generalize(atomicSentence.relation), atomicSentence.terms.map(_.generalize))

    def generalizeRelation: AtomicSentence = atomicSentence.copy(relation = generalize(atomicSentence.relation))

    private def generalize(relation: RelationConstant): RelationConstant = relation match {
      case Init | Next => True
      case Legal => Does
      case _ => relation
    }
  }

  implicit class LiteralDecorator(val literal: Literal) extends AnyVal {
    def substitute(v: Variable, c: ObjectConstant): Literal = literal match {
      case sentence: AtomicSentence => sentence.substitute(v, c)
      case Not(sentence) => Not(sentence.substitute(v, c))
      case Distinct(x, y) => Distinct(x.substitute(v, c), y.substitute(v, c))
    }
  }

  implicit class TermDecorator(val term: Term) extends AnyVal {
    def substitute(v: Variable, c: ObjectConstant): Term = term match {
      case u: Variable if u == v => c
      case f: AppliedFunction => f.copy(terms = f.terms.map(_.substitute(v, c)))
      case _ => term
    }

    def generalize: Term = term match {
      case ObjectConstant(_) | Variable(_) => Variable("x")
      case AppliedFunction(function, terms) => AppliedFunction(
        function, terms.map(_.generalize)
      )
    }
  }
}
