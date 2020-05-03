package simplyTyped

import simplyTyped.Term.{Annotated, Application, BoundVariable, FreeVariable, Inf, Lambda}

object Substitutions {

  implicit class RichInferrableTerm(term: InferrableTerm) {
    def substitute(i: Int, replacement: InferrableTerm): InferrableTerm = term match {
      case Annotated(term, typ) => Annotated(term.substitute(i, replacement), typ)
      case BoundVariable(j) => if (i == j) replacement else BoundVariable(j)
      case FreeVariable(name) => FreeVariable(name)
      case Application(function, argument) => Application(function.substitute(i, replacement), argument.substitute(i, replacement))
    }
  }

  implicit class RichCheckableTerm(term: CheckableTerm) {

    def substitute(i: Int, replacement: InferrableTerm): CheckableTerm = term match {
      case Inf(term) => Inf(term.substitute(i, replacement))
      case Lambda(body) => Lambda(body.substitute(i + 1, replacement))
    }

  }


}