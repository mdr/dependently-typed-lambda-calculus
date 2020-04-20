package simplyTyped

import scala.language.implicitConversions

sealed trait InferrableTerm {

  def apply(argument: CheckableTerm): InferrableTerm = Term.Application(this, argument)

  override def toString: String = PrettyPrinter.prettyPrint(this)

}

object Term {

  case class Annotated(term: CheckableTerm, typ: Type) extends InferrableTerm

  case class BoundVariable(n: Int) extends InferrableTerm

  object FreeVariable {

    def apply(name: String): FreeVariable = FreeVariable(Name.Global(name))

  }

  case class FreeVariable(name: Name) extends InferrableTerm

  case class Application(function: InferrableTerm, argument: CheckableTerm) extends InferrableTerm

  case class Inf(term: InferrableTerm) extends CheckableTerm

  case class Lambda(body: CheckableTerm) extends CheckableTerm

}

object CheckableTerm {

  implicit def inferrableToCheckable(term: InferrableTerm): CheckableTerm = Term.Inf(term)

}

sealed trait CheckableTerm {

  override def toString: String = PrettyPrinter.prettyPrint(this)

}
