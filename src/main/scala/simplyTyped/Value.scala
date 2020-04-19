package simplyTyped

sealed trait Value

object Value {

  def vfree(name: Name): Value = NeutralValue(FreeNeutral(name))

}

case class LambdaValue(function: Value => Value) extends Value

case class NeutralValue(value: Neutral) extends Value

sealed trait Neutral

case class FreeNeutral(name: Name) extends Neutral

case class AppNeutral(function: Neutral, value: Value) extends Neutral