package dependentlyTyped

sealed trait Value {

  override def toString: String = Quoter.quote(this).toString

  def apply(argument: Value) = Evaluator.apply(this, argument)

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

  case class Pi(argumentType: Value, dependentResultType: Value => Value) extends Value

  def freeVariable(name: Name): Value = Neutral(dependentlyTyped.Neutral.FreeVariable(name))

}

sealed trait Neutral

object Neutral {

  case class FreeVariable(name: Name) extends Neutral

  case class Application(function: Neutral, argument: Value) extends Neutral

  case class NatElim(motive: Value, zeroCase: Value, succCase: Value, n: Neutral) extends Neutral

  case class VecElim(elementType: Value, motive: Value, nilCase: Value, consCase: Value, length: Value, vector: Neutral) extends Neutral

}