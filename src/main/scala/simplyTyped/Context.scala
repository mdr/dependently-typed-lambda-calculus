package simplyTyped

sealed trait Kind

case object * extends Kind

sealed trait Info

case class HasKind(kind: Kind) extends Info

case class HasType(typ: Type) extends Info

object Context {

  val empty: Context = Context(Map.empty)

}

case class Context(infoByName: Map[Name, Info]) {
  def apply(name: Name): Option[Info] = infoByName.get(name)

  def withLocalType(i: Int, typ: Type): Context =
    Context(infoByName + (Name.Local(i) -> HasType(typ)))

  def withGlobalKind(name: String, kind: Kind): Context =
    Context(infoByName + (Name.Global(name) -> HasKind(kind)))

  def withGlobalType(name: String, typ: Type): Context =
    Context(infoByName + (Name.Global(name) -> HasType(typ)))

}