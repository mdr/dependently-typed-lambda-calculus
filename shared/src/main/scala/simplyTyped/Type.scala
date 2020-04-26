package simplyTyped

sealed trait Type {

  def ->(resultType: Type): FunctionType = FunctionType(this, resultType)

  def ::(term: CheckableTerm): InferrableTerm = Term.Annotated(term, this)

  override def toString: String = PrettyPrinter.prettyPrint(this)
}

object FreeType {
  def apply(name: String): FreeType = FreeType(Name.Global(name))
}

case class FreeType(name: Name) extends Type

case class FunctionType(argumentType: Type, resultType: Type) extends Type

