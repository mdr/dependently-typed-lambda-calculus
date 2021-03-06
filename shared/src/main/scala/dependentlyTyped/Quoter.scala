package dependentlyTyped

object Quoter {

  def quote(value: Value, bindersPassed: Int = 0): CheckableTerm =
    value match {
      case Value.Lambda(function) => Term.Lambda(quote(function(Value.freeVariable(Name.Quote(bindersPassed))), bindersPassed + 1))
      case Value.Neutral(value) => Term.Inf(quote(value, bindersPassed))
      case Value.* => Term.*
      case Value.Nat => Term.Nat
      case Value.Zero => Term.Zero
      case Value.Succ(term) => Term.Succ(quote(term, bindersPassed))
      case Value.Nil(elementType) => Term.Nil(quote(elementType, bindersPassed))
      case Value.Cons(elementType, length, head, tail) => Term.Cons(quote(elementType, bindersPassed), quote(length, bindersPassed), quote(head, bindersPassed), quote(tail, bindersPassed))
      case Value.Vec(elementType, length) => Term.Vec(quote(elementType, bindersPassed), quote(length, bindersPassed))
      case Value.Pi(argumentType, dependentResultType) =>
        val quotedArgumentType = quote(argumentType, bindersPassed)
        val quotedResultType = quote(dependentResultType.apply(Value.freeVariable(Name.Quote(bindersPassed))), bindersPassed + 1)
        Term.Pi(quotedArgumentType, quotedResultType)
      case Value.Fin(n) => Term.Fin(quote(n, bindersPassed))
      case Value.FZero(n) => Term.FZero(quote(n, bindersPassed))
      case Value.FSucc(n, term) => Term.FSucc(quote(n, bindersPassed), quote(term, bindersPassed))
      case Value.Eq(typ, left, right) => Term.Eq(quote(typ, bindersPassed), quote(left, bindersPassed), quote(right, bindersPassed))
      case Value.Refl(typ, value) => Term.Refl(quote(typ, bindersPassed), quote(value, bindersPassed))
    }

  private def quote(neutral: Neutral, bindersPassed: Int): InferrableTerm =
    neutral match {
      case Neutral.FreeVariable(name) => boundFree(name, bindersPassed)
      case Neutral.Application(function, value) => Term.Application(quote(function, bindersPassed), quote(value, bindersPassed))
      case Neutral.NatElim(motive, zeroCase, succCase, n) => Term.NatElim(quote(motive, bindersPassed), quote(zeroCase, bindersPassed), quote(succCase, bindersPassed), quote(n, bindersPassed))
      case Neutral.VecElim(elementType, motive, nilCase, consCase, length, vector) =>
        Term.VecElim(quote(elementType, bindersPassed), quote(motive, bindersPassed), quote(nilCase, bindersPassed), quote(consCase, bindersPassed), quote(length, bindersPassed), quote(vector, bindersPassed))
      case Neutral.FinElim(motive, zeroCase, succCase, n, fin) =>
        Term.FinElim(quote(motive, bindersPassed), quote(zeroCase, bindersPassed), quote(succCase, bindersPassed), quote(n, bindersPassed), quote(fin, bindersPassed))
      case Neutral.EqElim(typ, motive, reflCase, left, right, equality) =>
        Term.EqElim(quote(typ, bindersPassed), quote(motive, bindersPassed), quote(reflCase, bindersPassed), quote(left, bindersPassed), quote(right, bindersPassed), quote(equality, bindersPassed))
    }

  private def boundFree(name: Name, bindersPassed: Int): InferrableTerm = name match {
    case Name.Quote(n) => Term.BoundVariable(bindersPassed - n - 1)
    case _ => Term.FreeVariable(name)
  }

}
