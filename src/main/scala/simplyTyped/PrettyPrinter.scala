package simplyTyped

object PrettyPrinter {

  def prettyPrint(term: InferrableTerm): String =
    term match {
      case Term.Annotated(term, typ) => s"((${prettyPrint(term)}) :: ${prettyPrint(typ)})"
      case Term.BoundVariable(n) => n.toString
      case Term.FreeVariable(name) => prettyPrint(name)
      case Term.Application(function, argument) => s"(${prettyPrint(function)} ${prettyPrint(argument)})"
    }

   def prettyPrint(name: Name): String = {
    name match {
      case Name.Global(name) => name
      case Name.Local(n) => s"Local-$n"
      case Name.Quote(n) => s"Quote-$n"
    }
  }

  def prettyPrint(term: CheckableTerm): String =
    term match {
      case Term.Inf(term) => prettyPrint(term)
      case Term.Lambda(body) => s"(λ -> ${prettyPrint(body)})"
    }

  def prettyPrint(typ: Type): String =
    typ match {
      case FreeType(name) => prettyPrint(name)
      case FunctionType(argumentType, resultType) => s"(${prettyPrint(argumentType)} -> ${prettyPrint(resultType)})"
    }
}
