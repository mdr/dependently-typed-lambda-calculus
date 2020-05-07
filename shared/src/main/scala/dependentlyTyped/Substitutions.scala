package dependentlyTyped

import dependentlyTyped.Term._

object Substitutions {

  implicit class RichInferrableTerm(term: InferrableTerm) {
    def substitute(i: Int, replacement: InferrableTerm): InferrableTerm = term match {
      case Annotated(term, typ) => Annotated(term.substitute(i, replacement), typ)
      case BoundVariable(j) => if (i == j) replacement else BoundVariable(j)
      case FreeVariable(name) => FreeVariable(name)
      case Application(function, argument) => Application(function.substitute(i, replacement), argument.substitute(i, replacement))
      case Term.* => Term.*
      case Term.Nat => Term.Nat
      case Pi(argumentType, resultType) => Pi(argumentType.substitute(i, replacement), resultType.substitute(i + 1, replacement))
    }
  }

  implicit class RichCheckableTerm(term: CheckableTerm) {

    def substitute(i: Int, replacement: InferrableTerm): CheckableTerm = term match {
      case Inf(term) => Inf(term.substitute(i, replacement))
      case Lambda(body) => Lambda(body.substitute(i + 1, replacement))
    }

  }


}