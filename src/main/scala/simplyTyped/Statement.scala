package simplyTyped

sealed trait Statement

object Statement {

  case class Eval(term: InferrableTerm) extends Statement

  case class Let(name: String, term: InferrableTerm) extends Statement

  case class Assume(name: String, info: Info) extends Statement

}
