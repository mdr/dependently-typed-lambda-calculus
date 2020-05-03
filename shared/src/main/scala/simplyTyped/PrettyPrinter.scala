package simplyTyped

import scala.PartialFunction.cond
import Substitutions._

object PrettyPrinter {

  def prettyPrint(term: InferrableTerm, nameSupplier: NameSupplier = NameSupplier()): String =
    term match {
      case Term.Annotated(term, typ) => s"((${prettyPrint(term, nameSupplier)}) :: ${prettyPrint(typ)})"
      case Term.BoundVariable(n) => n.toString
      case Term.FreeVariable(name) => prettyPrint(name)
      case Term.Application(function, argument) =>
        val parensForArg = cond(argument) { case Term.Inf(Term.Application(_, _)) => true }
        s"${prettyPrint(function, nameSupplier)} ${maybeParens(parensForArg, prettyPrint(argument, nameSupplier))}"
    }

  def prettyPrint(name: Name): String = {
    name match {
      case Name.Global(name) => name
      case Name.Local(n) => s"Local-$n"
      case Name.Quote(n) => s"Quote-$n"
    }
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

        def getNames(n: Int, nameSupplier: NameSupplier = NameSupplier()): (Seq[String], NameSupplier) =
          if (n == 0) (Seq.empty, nameSupplier)
          else {
            val (names, nameSupplier2) = getNames(n - 1, nameSupplier)
            val (name, nameSupplier3) = nameSupplier2.getName(avoid = freeNames)
            (name +: names, nameSupplier3)
          }

        val (names, newNameSupplier) = getNames(numberOfLambdas, nameSupplier)
        val newBody = names.reverse.zipWithIndex.foldRight(ultimateBody) {
          case ((name, index), body) => body.substitute(index, Term.FreeVariable(Name.Global(name)))
        }
        s"(λ${names.mkString(" ")} → ${prettyPrint(newBody, newNameSupplier)})"
    }

  def prettyPrint(typ: Type): String =
    typ match {
      case FreeType(name) => prettyPrint(name)
      case FunctionType(argumentType, resultType) =>
        val parensForArgType = cond(argumentType) { case FunctionType(_, _) => true }
        s"${maybeParens(parensForArgType, prettyPrint(argumentType))} → ${prettyPrint(resultType)}"
    }

  private def maybeParens(parens: Boolean, s: String): String = if (parens) s"($s)" else s

}

case class NameSupplier(names: Seq[String] = "abcdefghijklmnopqrstuvwxyz".split("")) {
  def getName(avoid: Seq[String]): (String, NameSupplier) = {
    val availableNames = names.filterNot(avoid.contains)
    availableNames.head -> NameSupplier(availableNames.tail)
  }
}