package dependentlyTyped

sealed trait Value {

  override def toString: String = Quoter.quote(this).toString

}

object Value {

  case class Lambda(function: Value => Value) extends Value

  case class Neutral(value: dependentlyTyped.Neutral) extends Value

  case object * extends Value

  case class Pi(argumentType: Value, dependentResultType: Value => Value) extends Value

  def freeVariable(name: Name): Value = Neutral(dependentlyTyped.Neutral.FreeVariable(name))

}

sealed trait Neutral

object Neutral {

  case class FreeVariable(name: Name) extends Neutral

  case class Application(function: Neutral, argument: Value) extends Neutral

}