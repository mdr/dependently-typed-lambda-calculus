package simplyTyped

object Quoter {

  def quote(value: Value, bindersPassed: Int = 0): CheckableTerm =
    value match {
      case LambdaValue(function) => Lambda(quote(function(Value.vfree(Name.Quote(bindersPassed))), bindersPassed + 1))
      case NeutralValue(value) => Inf(neutralQuote(value, bindersPassed))
    }

  private def neutralQuote(neutral: Neutral, bindersPassed: Int): InferrableTerm = neutral match {
    case FreeNeutral(name) => boundFree(name, bindersPassed)
    case AppNeutral(function, value) => App(neutralQuote(function, bindersPassed), quote(value, bindersPassed))
  }

  private def boundFree(name: Name, bindersPassed: Int): InferrableTerm = name match {
    case Name.Quote(n) => BoundVariable(bindersPassed - n - 1)
    case _ => FreeVariable(name)
  }

}
