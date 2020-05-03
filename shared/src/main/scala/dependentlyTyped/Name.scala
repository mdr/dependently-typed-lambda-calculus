package dependentlyTyped

sealed trait Name {
  //  override def toString: String = PrettyPrinter.prettyPrint(this)
}

object Name {

  case class Global(name: String) extends Name

  case class Local(n: Int) extends Name

  case class Quote(n: Int) extends Name

}

