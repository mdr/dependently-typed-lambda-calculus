package dependentlyTyped

object Quoter {

  def quote(value: Value, bindersPassed: Int = 0): CheckableTerm =
    value match {
      case Value.Lambda(function) => Term.Lambda(quote(function(Value.freeVariable(Name.Quote(bindersPassed))), bindersPassed + 1))
      case Value.Neutral(value) => Term.Inf(quote(value, bindersPassed))
      case Value.* => Term.*
      case Value.Nat => Term.Nat
      case Value.Pi(argumentType, dependentResultType) =>
        val quotedArgumentType = quote(argumentType, bindersPassed)
        val quotedResultType = quote(dependentResultType.apply(Value.freeVariable(Name.Quote(bindersPassed))), bindersPassed + 1)
        Term.Pi(quotedArgumentType, quotedResultType)
    }

  private def quote(neutral: Neutral, bindersPassed: Int): InferrableTerm =
    neutral match {
      case Neutral.FreeVariable(name) => boundFree(name, bindersPassed)
      case Neutral.Application(function, value) => Term.Application(quote(function, bindersPassed), quote(value, bindersPassed))
    }

  private def boundFree(name: Name, bindersPassed: Int): InferrableTerm = name match {
    case Name.Quote(n) => Term.BoundVariable(bindersPassed - n - 1)
    case _ => Term.FreeVariable(name)
  }

}
