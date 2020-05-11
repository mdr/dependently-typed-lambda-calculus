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
      case Nat => Nat
      case Zero => Zero
      case Succ(term) => Succ(term.substitute(i, replacement))
      case NatElim(motive, zeroCase, succCase, n) => NatElim(motive.substitute(i, replacement), zeroCase.substitute(i, replacement), succCase.substitute(i, replacement), n.substitute(i, replacement))
      case Nil(elementType) => Nil(elementType.substitute(i, replacement))
      case Cons(elementType, length, head, tail) => Cons(elementType.substitute(i, replacement), length.substitute(i, replacement), head.substitute(i, replacement), tail.substitute(i, replacement))
      case Vec(elementType, length) => Vec(elementType.substitute(i, replacement), length.substitute(i, replacement))
      case VecElim(elementType, motive, nilCase, consCase, length, vector) =>
        VecElim(elementType.substitute(i, replacement), motive.substitute(i, replacement), nilCase.substitute(i, replacement), consCase.substitute(i, replacement), length.substitute(i, replacement), vector.substitute(i, replacement))
      case Fin(n) => Fin(n.substitute(i, replacement))
      case FZero(n) => FZero(n.substitute(i, replacement))
      case FSucc(n, term) => FSucc(n.substitute(i, replacement), term.substitute(i, replacement))
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