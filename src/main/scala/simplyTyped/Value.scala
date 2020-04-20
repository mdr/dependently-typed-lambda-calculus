package simplyTyped

sealed trait Value {

  override def toString: String = Quoter.quote(this).toString

}

object Value {

  case class Lambda(function: Value => Value) extends Value

  case class Neutral(value: simplyTyped.Neutral) extends Value

  def freeVariable(name: Name): Value = Neutral(simplyTyped.Neutral.FreeVariable(name))

}

sealed trait Neutral

object Neutral {

  case class FreeVariable(name: Name) extends Neutral

  case class Application(function: Neutral, argument: Value) extends Neutral

}