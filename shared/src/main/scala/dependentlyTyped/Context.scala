package dependentlyTyped

object Context {

  val empty: Context = Context(Map.empty)

}

case class Context(infoByName: Map[Name, Type]) {
  def apply(name: Name): Option[Type] = infoByName.get(name)

  def apply(name: String): Option[Type] = infoByName.get(Name.Global(name))

  def withLocalType(i: Int, typ: Type): Context =
    Context(infoByName + (Name.Local(i) -> typ))

  def withGlobal(name: String, info: Type): Context =
    Context(infoByName + (Name.Global(name) -> info))

  def withGlobalType(name: String, typ: Type): Context = withGlobal(name, typ)

}