package dependentlyTyped

sealed trait Statement

object Statement {

  case class Eval(term: InferrableTerm) extends Statement

  case class Let(name: String, term: InferrableTerm) extends Statement

  case class Assume(assumptions: Seq[Assumption]) extends Statement

}

case class Assumption(name: String, typ: InferrableTerm)
