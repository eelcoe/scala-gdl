package gdl.propnet

import gdl.lang._

import scala.util.{Failure, Success, Try}
import scala.collection.mutable

class InvalidTermException(message: String) extends Exception

object Flattener {
  private val fillerVar = Variable("foo")

  def flatten(description: Description): Description = {
    val domains = initializeDomains(description)
    updateDomains(domains)
    getAllInstantiations(domains, description)
  }

  private def findGenericForm(term: Term): Term = term match {
    case _: ObjectConstant => fillerVar
    case _: Variable => fillerVar
    case AppliedFunction(function, terms) => AppliedFunction(
      function, terms.map(findGenericForm)
    )
  }

  private def findGenericForm(sentence: AtomicSentence): AtomicSentence = {
    val relation = sentence.relation match {
      case Init | Next => True
      case Legal => Does
      case _ => sentence.relation
    }
    AtomicSentence(relation, sentence.terms.map(findGenericForm))
  }

  private def getConstantAndVariableList(expression: Expression): List[Term] = expression match {
    case objectConstant: ObjectConstant => List(objectConstant)
    case variable: Variable => List(variable)
    case AppliedFunction(_, terms) => terms.toList.flatMap(getConstantAndVariableList)
    case AtomicSentence(_, terms) => terms.toList.flatMap(getConstantAndVariableList)
    case Not(AtomicSentence(_, terms)) => terms.toList.flatMap(getConstantAndVariableList)
  }

  private def getConstantList(expression: Expression): Try[List[ObjectConstant]] = expression match {
    case objectConstant: ObjectConstant => Success(List(objectConstant))
    case _: Variable => Failure(new InvalidTermException("Called getConstantList on something containing a variable."))
    case AppliedFunction(_, terms) => flattenTryList(terms.toList.map(getConstantList))
    case AtomicSentence(_, terms) => flattenTryList(terms.toList.map(getConstantList))
    case Not(AtomicSentence(_, terms)) => flattenTryList(terms.toList.map(getConstantList))
  }

  private def flattenTryList[T](lists: List[Try[List[T]]]): Try[List[T]] =
    lists.foldLeft[Try[List[T]]](Success(List())) { case (successes, triedList) =>
      triedList match {
        case Success(list) => successes.map(_ ++ list)
        case failure: Failure[_] => failure
      }
    }

  def initializeDomains(description: Description): Map[AtomicSentence, Domain] = {
    val domains = mutable.HashMap[AtomicSentence, Domain]()

    for (rule <- description.rules) {
      //if (rule.head.relation != Base)
      val head = rule.head
      val generified = findGenericForm(head)

      val dom = domains.getOrElseUpdate(generified, new Domain(generified, head))

      if (rule.body.size == 0) {
        val instantiation = getConstantList(head).getOrElse {
          println("Rule without body (i.e. a fact) must not contain variables")
          List()
        }

        dom.assignments.add(instantiation)
      } else {
        val productionTemplate = getConstantAndVariableList(head)
        val conditions = rule.body.collect {
          case sentence: AtomicSentence => Condition(findGenericForm(sentence), getConstantAndVariableList(sentence))
        }.toList

        dom.ruleRefs.add(RuleReference(productionTemplate, conditions, rule))
      }
    }

    domains.toMap
  }

  def updateDomains(domains: Map[AtomicSentence, Domain]): Unit = {
    var changedSomething = true
    var itrNum = 0
    var lastUpdatedDomains = domains.values.to[mutable.HashSet]

    while (changedSomething) {
      val currUpdatedDomains = mutable.HashSet[Domain]()
      changedSomething = false

      var rulesConsidered = 0
      for {
        d <- domains.values
        ruleRef <- d.ruleRefs
      } {
        // if there is a domain for one or more conditions of this rule
        // and the domain was updated in the last iteration
        if (ruleRef.conditions.exists(c => lastUpdatedDomains.exists(domains.get(c.key).contains))) {
          rulesConsidered += 1

          val assignments = findSatisfyingAssignments(domains, ruleRef)
          for (assignment <- assignments) {
            if (!d.assignments.contains(assignment)) {
              currUpdatedDomains.add(d)
              d.assignments.add(assignment)
              changedSomething = true
            }
          }
        }
      }
      itrNum += 1
      lastUpdatedDomains = currUpdatedDomains
    }
  }

  def findSatisfyingAssignments(domains: Map[AtomicSentence, Domain], ruleReference: RuleReference): Set[List[ObjectConstant]] = {
    val instantiations = findSatisfyingInstantiations(domains, ruleReference.conditions)

    if (instantiations.isEmpty) {
      val staticAssignment = ruleReference.productionTemplate.collect {
        case objectConstant: ObjectConstant => objectConstant
      }

      if (staticAssignment.size == ruleReference.productionTemplate.size) {
        Set(staticAssignment)
      } else {
        Set()
      }
    } else {
      instantiations.map { instantiation =>
        ruleReference.productionTemplate.collect {
          case objectConstant: ObjectConstant => objectConstant
          case variable: Variable => instantiation(variable)
        }
      }
    }
  }

  def findSatisfyingInstantiations(domains: Map[AtomicSentence, Domain], conditions: List[Condition], instantiation: Map[Variable, ObjectConstant] = Map[Variable, ObjectConstant]()): Set[Map[Variable, ObjectConstant]] = {
    conditions match {
      case Nil => Set(instantiation)
      case condition :: remainingConditions => {
        val satisfyingAssignments = (for {
          domain <- domains.get(condition.key).toList
          (term, i) <- condition.template.zipWithIndex
          index <- domain.indices.lift(i)
          assignments <- term match {
            case c: ObjectConstant => index.get(c)
            case v: Variable if instantiation.contains(v) => index.get(instantiation(v))
            case _: Variable => Some(domain.assignments.toSet)
            case _ => None
          }
        } yield assignments)
          .reduceOption(_ intersect _).getOrElse(Set[List[ObjectConstant]]())

        satisfyingAssignments.flatMap { assignment: List[ObjectConstant] =>

          val newInstantiations = (condition.template zip assignment).collect {
            case (variable: Variable, constant: ObjectConstant) => (variable, constant)
          }.toMap

          findSatisfyingInstantiations(domains, remainingConditions, instantiation ++ newInstantiations)
        }
      }
    }
  }

  private def getAllInstantiations(domains: Map[AtomicSentence, Domain], description: Description): Description = {
    val facts = description.rules.filter(_.body.size == 0).toSet.toSeq
    val instantiatedRules = for {
      (k, d) <- domains
      r <- d.ruleRefs
      varInstantiation <- findSatisfyingInstantiations(domains, r.conditions)
    } yield getInstantiation(r.rule, varInstantiation)

    Description(facts ++ instantiatedRules)
  }

  private def getInstantiation(rule: Rule, varInstantiation: Map[Variable, ObjectConstant]): Rule =
    new Instantiation(varInstantiation)(rule)

  private class Instantiation(varInstantiation: Map[Variable, ObjectConstant]) {
    def apply(rule: Rule): Rule =
      rule.copy(head = replaceSentence(rule.head), body = rule.body.map(replaceLiteral))

    private def replaceSentence(sentence: AtomicSentence): AtomicSentence =
      sentence.copy(terms = sentence.terms.map(replaceTerm))

    private def replaceLiteral(literal: Literal): Literal = literal match {
      case AtomicSentence(relation, terms) => AtomicSentence(relation, terms.map(replaceTerm))
      case Not(sentence) => Not(replaceSentence(sentence))
    }

    private def replaceTerm(term: Term): Term = term match {
      case AppliedFunction(function, terms) => AppliedFunction(function, terms.map(replaceTerm))
      case objectConstant: ObjectConstant => objectConstant
      case variable: Variable => varInstantiation(variable)
    }
  }
}

case class Condition(key: AtomicSentence, template: List[Term])
case class RuleReference(productionTemplate: List[Term], conditions: List[Condition], rule: Rule)

class Domain(key: AtomicSentence, value: AtomicSentence) {
  val assignments = mutable.Set[List[ObjectConstant]]()
  val ruleRefs = mutable.Set[RuleReference]()

  def indices: List[Map[ObjectConstant, Set[List[ObjectConstant]]]] = assignments.toList
    .map(_.zipWithIndex).flatten
    .groupBy(_._2).mapValues(_.map(_._1)).toList
    .sortBy(_._1).map((findAssignments _).tupled)

  private def findAssignments(position: Int, constants: List[ObjectConstant]): Map[ObjectConstant, Set[List[ObjectConstant]]] =
    constants.map { objectConstant =>
      (objectConstant, assignments.toSet.filter(_.lift(position).contains(objectConstant)))
    }.toMap
}
