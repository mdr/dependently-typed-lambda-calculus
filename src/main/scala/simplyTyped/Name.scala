package simplyTyped

sealed trait Name

object Name {

  case class Global(name: String) extends Name

  case class Local(n: Int) extends Name

  case class Quote(n: Int) extends Name

}

