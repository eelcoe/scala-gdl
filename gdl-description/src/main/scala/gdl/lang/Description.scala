package gdl.lang

sealed trait Expression {
  def isGround: Boolean
}

sealed trait Term extends Expression

case class ObjectConstant(name: String) extends Term {
  require(name.size > 0, "the name of a constant must not be empty")
  override def isGround: Boolean = true
}

case class Variable(name: String) extends Term {
  require(name.size > 0, "the name of a variable must not be empty")
  override def isGround: Boolean = false
}

case class FunctionConstant(name: String, arity: Int) {
  require(name.size > 0, "the name of a function must not be empty")
  require(arity > 0, "the arity of a function must be greater than zero")
}
case class AppliedFunction(function: FunctionConstant, terms: Seq[Term]) extends Term {
  require(function.arity == terms.size, "the number of terms to apply the function to must be the same as its arity")
  override def isGround = terms.forall(_.isGround)

  def collectVariables: Set[Variable] = terms.flatMap {
    case _: ObjectConstant => None
    case v: Variable => Some(v)
    case f: AppliedFunction => f.collectVariables
  }.toSet
}
object AppliedFunction {
  def apply(function: String, args: Seq[Term]): AppliedFunction = AppliedFunction(FunctionConstant(function, args.size), args)
}

case class RelationConstant(name: String, arity: Int) {
  require(name.size > 0, "the name of a relation must not be empty")
}
object Does extends RelationConstant("does", 2) {
  def apply(player: Term, move: Term) = AtomicSentence(this, Seq(player, move))
}
object Goal extends RelationConstant("goal", 2) {
  def apply(player: Term, score: Term) = AtomicSentence(this, Seq(player, score))
}
object Init extends RelationConstant("init", 1) {
  def apply(state: Term) = AtomicSentence(this, Seq(state))
}
object Legal extends RelationConstant("legal", 2) {
  def apply(player: Term, move: Term) = AtomicSentence(this, Seq(player, move))
}
object Next extends RelationConstant("next", 1) {
  def apply(state: Term) = AtomicSentence(this, Seq(state))
}
object Role extends RelationConstant("role", 1) {
  def apply(player: Term) = AtomicSentence(this, Seq(player))
}
object Terminal extends RelationConstant("terminal", 0) {
  def apply() = AtomicSentence(this, Seq())
}
object True extends RelationConstant("true", 1) {
  def apply(state: Term) = AtomicSentence(this, Seq(state))
}

sealed trait Literal extends Expression {
  def relation: RelationConstant
  def collectVariables: Set[Variable]
}
case class AtomicSentence(relation: RelationConstant, terms: Seq[Term]) extends Literal {
  require(relation != Distinct, "distinct is a reserved name")
  require(relation.arity == terms.size, "the number of terms to apply the relation to must be the same as its arity")

  override def isGround = terms.forall(_.isGround)
  override def collectVariables: Set[Variable] = terms.flatMap {
    case _: ObjectConstant => Seq()
    case v: Variable => Seq(v)
    case f: AppliedFunction => f.collectVariables
  }.toSet
}
object AtomicSentence {
  def apply(relation: String): AtomicSentence = apply(relation, Seq())
  def apply(relation: String, args: Seq[Term]): AtomicSentence =
    AtomicSentence(RelationConstant(relation, args.size), args)
}
case class Not(atomicSentence: AtomicSentence) extends Literal {
  override def isGround = atomicSentence.isGround
  override def relation: RelationConstant = atomicSentence.relation
  override def collectVariables: Set[Variable] = atomicSentence.collectVariables
}

case class Distinct(x: Term, y: Term) extends Literal {
  override def isGround = x.isGround && y.isGround
  override def relation: RelationConstant = Distinct
  override def collectVariables: Set[Variable] = Seq(x, y).flatMap {
    case _: ObjectConstant => Seq()
    case v: Variable => Seq(v)
    case f: AppliedFunction => f.collectVariables
  }.toSet
}
object Distinct extends RelationConstant("distinct", 2)

case class Rule(head: AtomicSentence, body: Seq[Literal]) {
  require(!bodyContainsRelation(Init), "the `init` relation may only appear in the head of a rule, not in the body")
  require(!bodyContainsRelation(Next), "the `next` relation may only appear in the head of a rule, not in the body")

  if (head.relation == Role)
    require(body.isEmpty, "the `role` relation may only appear in the head of a rule when the body is empty")

  require(head.relation != Does, "the `does` relation may only appear in the body of a rule, not in the head")
  require(head.relation != True, "the `true` relation may only appear in the body of a rule, not in the head")

  require(isSafe, s"""
    all variables in the head and negative literals must also appear in a positive literal
    rule for ${head.relation.name}
    variables in head: ${variablesInHead.map(_.name).mkString(", ")}
    variables in negative literals: ${variablesInNegativeLiterals.map(_.name).mkString(", ")}
    variables in positive literals: ${variablesInPositiveLiterals.map(_.name).mkString(", ")}
  """.stripMargin)

  private def bodyContainsRelation(relation: RelationConstant): Boolean = body.collect({
    case AtomicSentence(rel, _) => rel
    case Not(AtomicSentence(rel, _)) => rel
  }).contains(relation)

  private def isSafe: Boolean = (variablesInHead union variablesInNegativeLiterals) subsetOf variablesInPositiveLiterals
  private def variablesInHead: Set[Variable] = head.collectVariables

  private def variablesInNegativeLiterals: Set[Variable] = body.flatMap {
    case _: AtomicSentence => Seq()
    case neg: Not => neg.collectVariables
    case dis: Distinct => dis.collectVariables
  }.toSet

  private def variablesInPositiveLiterals: Set[Variable] = body.flatMap {
    case pos: AtomicSentence => pos.collectVariables
    case _: Not => Seq()
    case _: Distinct => Seq()
  }.toSet
}

case class Description(rules: Seq[Rule])
