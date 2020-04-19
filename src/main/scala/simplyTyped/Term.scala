package simplyTyped

sealed trait InferrableTerm {

  def apply(argument: CheckableTerm): InferrableTerm = App(this, argument)

}

case class Ann(term: CheckableTerm, typ: Type) extends InferrableTerm

case class Bound(n: Int) extends InferrableTerm

object Free {

  def apply(name: String): Free = Free(Name.Global(name))

}

case class Free(name: Name) extends InferrableTerm

case class App(function: InferrableTerm, argument: CheckableTerm) extends InferrableTerm

sealed trait CheckableTerm {

}

case class Inf(term: InferrableTerm) extends CheckableTerm

case class Lam(body: CheckableTerm) extends CheckableTerm

