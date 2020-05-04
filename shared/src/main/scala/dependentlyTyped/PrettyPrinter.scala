package dependentlyTyped

import scala.PartialFunction.cond
import Substitutions._

import scala.annotation.tailrec

object PrettyPrinter {

  def prettyPrint(term: InferrableTerm, nameSupplier: NameSupplier = NameSupplier()): String =
    term match {
      case Term.Annotated(term, typ) => s"(${prettyPrint(term, nameSupplier)} :: ${prettyPrint(typ, nameSupplier)})"
      case Term.BoundVariable(n) => n.toString
      case Term.FreeVariable(name) => prettyPrint(name)
      case Term.Application(function, argument) =>
        val parensForArg = cond(argument) { case Term.Inf(Term.Application(_, _)) => true }
        s"${prettyPrint(function, nameSupplier)} ${maybeParens(parensForArg, prettyPrint(argument, nameSupplier))}"
      case Term.* => "*"
      case Term.Pi(argumentType, resultType) =>
        if (!containsBoundVariable(resultType, 0)) {
          val parensForArgType = cond(argumentType) {
            case Term.Inf(Term.Annotated(_, _)) => true
            case Term.Inf(Term.Pi(_, _)) => true
            case Term.Lambda(_) => true
          }
          val parensForResultType = cond(resultType) {
            case Term.Inf(Term.Annotated(_, _)) => true
            case Term.Lambda(_) => true
          }
          s"${maybeParens(parensForArgType, prettyPrint(argumentType, nameSupplier))} -> ${maybeParens(parensForResultType, prettyPrint(resultType, nameSupplier))}"
        } else {
          val (argumentTypes, ultimateResultType) = getPis(term)
          val freeNames = resultType.freeVariables.collect { case Name.Global(name) => name }
          val (names, newNameSupplier) = nameSupplier.getNames(argumentTypes.size, avoid = freeNames)
          val newUltimateResultType = names.reverse.zipWithIndex.foldRight(ultimateResultType) {
            case ((name, index), body) => body.substitute(index, Term.FreeVariable(Name.Global(name)))
          }
          val prettyPrintedArgs = names.zip(argumentTypes).map {
            case (name, argumentType) => s"($name :: ${prettyPrint(argumentType, newNameSupplier)})"
          }.mkString(" ")
          s"∀ $prettyPrintedArgs . ${prettyPrint(newUltimateResultType, newNameSupplier)}"
        }
    }

  @tailrec
  private def containsBoundVariable(term: CheckableTerm, n: Int): Boolean =
    term match {
      case Term.Inf(term) => containsBoundVariable(term, n)
      case Term.Lambda(body) => containsBoundVariable(body, n + 1)
    }

  private def containsBoundVariable(term: InferrableTerm, n: Int): Boolean =
    term match {
      case Term.Annotated(term, typ) => containsBoundVariable(term, n) || containsBoundVariable(typ, n)
      case Term.* => false
      case Term.Pi(argumentType, resultType) => containsBoundVariable(argumentType, n) || containsBoundVariable(resultType, n + 1)
      case Term.BoundVariable(m) => n == m
      case Term.FreeVariable(_) => false
      case Term.Application(function, argument) => containsBoundVariable(function, n) || containsBoundVariable(argument, n)
    }

  private def getPis(term: CheckableTerm): (Seq[CheckableTerm], CheckableTerm) =
    term match {
      case Term.Inf(subterm) => getPis(subterm)
      case Term.Lambda(_) => Seq.empty -> term
    }

  private def getPis(term: InferrableTerm): (Seq[CheckableTerm], CheckableTerm) =
    term match {
      case Term.Pi(argumentType, resultType) if containsBoundVariable(resultType, 0) =>
        val (argumentTypes, ultimateResultType) = getPis(resultType)
        (argumentType +: argumentTypes) -> ultimateResultType
      case _ => Seq.empty -> term
    }

  def prettyPrint(name: Name): String =
    name match {
      case Name.Global(name) => name
      case Name.Local(n) => s"Local-$n"
      case Name.Quote(n) => s"Quote-$n"
    }

  private def getLambdas(term: CheckableTerm): (Int, CheckableTerm) =
    term match {
      case Term.Inf(_) => 0 -> term
      case Term.Lambda(body) =>
        val (n, ultimateBody) = getLambdas(body)
        n + 1 -> ultimateBody
    }

  def prettyPrint(term: CheckableTerm, nameSupplier: NameSupplier): String =
    term match {
      case Term.Inf(term) => prettyPrint(term, nameSupplier)
      case Term.Lambda(_) =>
        val (numberOfLambdas, ultimateBody) = getLambdas(term)
        val freeNames = ultimateBody.freeVariables.collect { case Name.Global(name) => name }

        val (names, newNameSupplier) = nameSupplier.getNames(numberOfLambdas, avoid = freeNames)
        val newBody = names.reverse.zipWithIndex.foldRight(ultimateBody) {
          case ((name, index), body) => body.substitute(index, Term.FreeVariable(Name.Global(name)))
        }
        s"(λ${names.mkString(" ")} -> ${prettyPrint(newBody, newNameSupplier)})"
    }

  private def maybeParens(parens: Boolean, s: String): String = if (parens) s"($s)" else s

}
