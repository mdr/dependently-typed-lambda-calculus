package simplyTyped

import scala.language.implicitConversions

sealed trait InferrableTerm {

  def apply(argument: CheckableTerm): InferrableTerm = App(this, argument)

}

case class Ann(term: CheckableTerm, typ: Type) extends InferrableTerm

case class BoundVariable(n: Int) extends InferrableTerm

object FreeVariable {

  def apply(name: String): FreeVariable = FreeVariable(Name.Global(name))

}

case class FreeVariable(name: Name) extends InferrableTerm

case class App(function: InferrableTerm, argument: CheckableTerm) extends InferrableTerm

object CheckableTerm {

  implicit def inferrableToCheckable(term: InferrableTerm): CheckableTerm = Inf(term)

}

sealed trait CheckableTerm {

}

case class Inf(term: InferrableTerm) extends CheckableTerm

case class Lambda(body: CheckableTerm) extends CheckableTerm

