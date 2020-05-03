package dependentlyTyped

import scala.language.implicitConversions

sealed trait InferrableTerm {

  def apply(argument: CheckableTerm): InferrableTerm = Term.Application(this, argument)

  override def toString: String = PrettyPrinter.prettyPrint(this)

  // TODO: verify this makes snse
  def freeVariables: Seq[Name]

}

object Term {

  case class Annotated(term: CheckableTerm, typ: CheckableTerm) extends InferrableTerm {
    override def freeVariables: Seq[Name] = term.freeVariables ++ typ.freeVariables
  }

  case object * extends InferrableTerm {
    override def freeVariables: Seq[Name] = Seq.empty
  }

  case class Pi(argumentType: CheckableTerm, resultType: CheckableTerm) extends InferrableTerm {
    override def freeVariables: Seq[Name] = argumentType.freeVariables ++ resultType.freeVariables
  }

  case class BoundVariable(n: Int) extends InferrableTerm {
    override def freeVariables: Seq[Name] = Seq.empty
  }

  object FreeVariable {
    def apply(name: String): FreeVariable = FreeVariable(Name.Global(name))
  }

  case class FreeVariable(name: Name) extends InferrableTerm {
    override def freeVariables: Seq[Name] = Seq(name)
  }

  case class Application(function: InferrableTerm, argument: CheckableTerm) extends InferrableTerm {
    override def freeVariables: Seq[Name] = function.freeVariables ++ argument.freeVariables
  }

  case class Inf(term: InferrableTerm) extends CheckableTerm {
    override def freeVariables: Seq[Name] = term.freeVariables
  }

  case class Lambda(body: CheckableTerm) extends CheckableTerm {
    override def freeVariables: Seq[Name] = body.freeVariables
  }

}

object CheckableTerm {

  implicit def inferrableToCheckable(term: InferrableTerm): CheckableTerm = Term.Inf(term)

}

sealed trait CheckableTerm {

  override def toString: String = PrettyPrinter.prettyPrint(this, NameSupplier())

  def freeVariables: Seq[Name]

}
