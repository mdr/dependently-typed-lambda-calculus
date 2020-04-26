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

  def apply(name: String): Option[Info] = infoByName.get(Name.Global(name))

  def withLocalType(i: Int, typ: Type): Context =
    Context(infoByName + (Name.Local(i) -> HasType(typ)))

  def withGlobal(name: String, info: Info): Context =
    Context(infoByName + (Name.Global(name) -> info))

  def withGlobalKind(name: String, kind: Kind): Context = withGlobal(name, HasKind(kind))

  def withGlobalType(name: String, typ: Type): Context = withGlobal(name, HasType(typ))

}