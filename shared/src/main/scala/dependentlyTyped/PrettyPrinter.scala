package dependentlyTyped

import scala.PartialFunction.cond
import Substitutions._

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
      case Term.Pi(_, resultType) =>
        val (argumentTypes, ultimateResultType) = getPis(term)
        val freeNames = resultType.freeVariables.collect { case Name.Global(name) => name }
        val (names, newNameSupplier) = nameSupplier.getNames(argumentTypes.size, avoid = freeNames)
        val newUltimateResultType = names.reverse.zipWithIndex.foldRight(ultimateResultType) {
          case ((name, index), body) => body.substitute(index, Term.FreeVariable(Name.Global(name)))
        }
        val prettyPrintedArgs = names.zip(argumentTypes).map {
          case (name, argumentType) => s"($name :: ${prettyPrint(argumentType, newNameSupplier)})"
        }.mkString(" ")
        s"(∀ $prettyPrintedArgs . ${prettyPrint(newUltimateResultType, newNameSupplier)})"
    }

  private def getPis(term: CheckableTerm): (Seq[CheckableTerm], CheckableTerm) =
    term match {
      case Term.Inf(subterm) => getPis(subterm)
      case Term.Lambda(_) => Seq.empty -> term
    }

  private def getPis(term: InferrableTerm): (Seq[CheckableTerm], CheckableTerm) =
    term match {
      case Term.Pi(argumentType, resultType) =>
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
