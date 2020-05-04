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
        val parensForFunction = cond(function) {
          case Term.Pi(_, _) => true
        }
        val parensForArg = cond(argument) {
          case Term.Inf(Term.Application(_, _)) => true
          case Term.Inf(Term.Pi(_, _)) => true
        }
        val prettyPrintedFunction = maybeParens(parensForFunction, prettyPrint(function, nameSupplier))
        val prettyPrintedArgument = maybeParens(parensForArg, prettyPrint(argument, nameSupplier))
        s"$prettyPrintedFunction $prettyPrintedArgument"
      case Term.* => "*"
      case Term.Pi(argumentType, resultType) =>
        if (!containsBoundVariable(resultType, 0)) {
          prettyPrintFunctionType(argumentType, resultType, nameSupplier)
        } else {
          prettyPrintPiType(term, nameSupplier)
        }
    }

  private def prettyPrintPiType(term: InferrableTerm, nameSupplier: NameSupplier): String = {
    val PiCollectorResult(traversedPis, ultimateResultType, newNameSupplier) = collectPis(term, nameSupplier)
    val prettyPrintedArgs = traversedPis.map { case TraversedPi(argumentName, argumentType) =>
      s"($argumentName :: ${prettyPrint(argumentType, newNameSupplier)})"
    }.mkString(" ")
    s"∀ $prettyPrintedArgs . ${prettyPrint(ultimateResultType, newNameSupplier)}"
  }

  private def prettyPrintFunctionType(argumentType: CheckableTerm, resultType: CheckableTerm, nameSupplier: NameSupplier): String = {
    val parensForArgType = cond(argumentType) {
      case Term.Inf(Term.Annotated(_, _)) => true
      case Term.Inf(Term.Pi(_, _)) => true
      case Term.Lambda(_) => true
    }
    val parensForResultType = cond(resultType) {
      case Term.Inf(Term.Annotated(_, _)) => true
      case Term.Lambda(_) => true
    }
    val prettyPrintedArgumentType = maybeParens(parensForArgType, prettyPrint(argumentType, nameSupplier))
    val prettyPrintedResultType = maybeParens(parensForResultType, prettyPrint(resultType, nameSupplier))
    s"$prettyPrintedArgumentType -> $prettyPrintedResultType"
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

  case class TraversedPi(argumentName: String, argType: CheckableTerm)

  case class PiCollectorResult(traversedPis: Seq[TraversedPi], ultimateResultType: CheckableTerm, nameSupplier: NameSupplier) {
    def prepend(traversedPi: TraversedPi): PiCollectorResult = copy(traversedPis = traversedPi +: traversedPis)
  }

  private def collectPis(term: CheckableTerm, nameSupplier: NameSupplier): PiCollectorResult =
    term match {
      case Term.Inf(term) => collectPis(term, nameSupplier)
      case Term.Lambda(_) => PiCollectorResult(Seq.empty, term, nameSupplier)
    }

  private def collectPis(term: InferrableTerm, nameSupplier: NameSupplier): PiCollectorResult =
    term match {
      case Term.Pi(argumentType, resultType) if containsBoundVariable(resultType, 0) =>
        val (argName, newNameSupplier) = nameSupplier.getName(avoid = getFreeVariables(resultType))
        val traversedPi = TraversedPi(argName, argumentType)
        val rewrittenResultType = resultType.substitute(0, Term.FreeVariable(Name.Global(argName)))
        collectPis(rewrittenResultType, newNameSupplier).prepend(traversedPi)
      case _ => PiCollectorResult(Seq.empty, term, nameSupplier)
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
        val freeNames = getFreeVariables(ultimateBody)

        val (names, newNameSupplier) = nameSupplier.getNames(numberOfLambdas, avoid = freeNames)
        val newBody = names.reverse.zipWithIndex.foldRight(ultimateBody) {
          case ((name, index), body) => body.substitute(index, Term.FreeVariable(Name.Global(name)))
        }
        s"(λ${names.mkString(" ")} -> ${prettyPrint(newBody, newNameSupplier)})"
    }

  private def getFreeVariables(ultimateBody: CheckableTerm): Seq[String] =
    ultimateBody.freeVariables.collect { case Name.Global(name) => name }

  private def maybeParens(parens: Boolean, s: String): String = if (parens) s"($s)" else s

}
