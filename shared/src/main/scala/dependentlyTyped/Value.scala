package dependentlyTyped

sealed trait Value {

  override lazy val toString: String = Quoter.quote(this).toString

  def apply(argument: Value): Type = Evaluator.apply(this, argument)

}

object Value {

  case class Lambda(function: Value => Value) extends Value

  case class Neutral(value: dependentlyTyped.Neutral) extends Value

  case object * extends Value

  case object Nat extends Value

  case object Zero extends Value

  case class Succ(value: Value) extends Value

  case class Nil(elementType: Value) extends Value

  case class Cons(elementType: Value, length: Value, head: Value, tail: Value) extends Value

  case class Vec(elementType: Value, length: Value) extends Value

  case class Fin(n: Value) extends Value

  case class FZero(n: Value) extends Value

  case class FSucc(n: Value, value: Value) extends Value

  case class Eq(typ: Value, left: Value, right: Value) extends Value

  case class Refl(typ: Value, value: Value) extends Value

  case class Pi(argumentType: Value, dependentResultType: Value => Value) extends Value

  def freeVariable(name: Name): Value = Neutral(dependentlyTyped.Neutral.FreeVariable(name))

}

sealed trait Neutral

object Neutral {

  case class FreeVariable(name: Name) extends Neutral

  case class Application(function: Neutral, argument: Value) extends Neutral

  case class NatElim(motive: Value, zeroCase: Value, succCase: Value, n: Neutral) extends Neutral

  case class VecElim(elementType: Value, motive: Value, nilCase: Value, consCase: Value, length: Value, vector: Neutral) extends Neutral

  case class FinElim(motive: Value, zeroCase: Value, succCase: Value, n: Value, fin: Neutral) extends Neutral

  case class EqElim(typ: Value, motive: Value, reflCase: Value, left: Value, right: Value, equality: Neutral) extends Neutral

}