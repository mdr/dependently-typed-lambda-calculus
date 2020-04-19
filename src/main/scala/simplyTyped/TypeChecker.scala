package simplyTyped

import scala.annotation.tailrec

object TypeChecker {

  type Result[A] = Either[String, A]

  private def throwError(error: String) = Left(error)

  def checkKind(context: Context, typ: Type, k: Kind): Result[Unit] =
    typ match {
      case FreeType(name) => context(name) match {
        case Some(HasKind(*)) => Right(())
        case Some(HasType(_)) => throwError(s"type $name has type instead of kind")
        case None => throwError(s"unknown identifier $name")
      }
      case FunctionType(functionType, argumentType) =>
        for {
          _ <- checkKind(context, functionType, *)
          _ <- checkKind(context, argumentType, *)
        } yield ()
    }

  def inferType(term: InferrableTerm, Γ: Context, i: Int = 0): Result[Type] =
    term match {
      case Ann(term, typ) =>
        for {
          _ <- checkKind(Γ, typ, *)
          _ <- checkType(term, typ, Γ, i)
        } yield typ
      case Free(name) =>
        Γ(name) match {
          case Some(HasType(typ)) => Right(typ)
          case Some(HasKind(_)) => throwError(s"$name unexpectedly has a kind, not a type")
          case None => throwError(s"Unknown identifier: $name")
        }
      case App(function, argument) =>
        for {
          functionType <- inferType(function, Γ, i)
          resultType <- functionType match {
            case FunctionType(argumentType, resultType) =>
              for {
                _ <- checkType(argument, argumentType, Γ, i)
              } yield resultType
            case FreeType(_) => throwError("Illegal application")
          }
        } yield resultType
      case Bound(_) => throwError("Unexpected Bound term in type checking")
    }

  @tailrec
  def checkType(term: CheckableTerm, expectedType: Type, Γ: Context, i: Int): Result[Unit] =
    term match {
      case Inf(term) =>
        for {
          inferredType <- inferType(term, Γ, i)
          _ <- if (inferredType == expectedType) Right(()) else throwError(s"Type mismatch. Expected type '$expectedType', but was inferred as '$inferredType'.")
        } yield ()
      case Lam(body) => expectedType match {
        case FunctionType(argumentType, resultType) =>
          val substitutedTerm = substitute(0, Free(Name.Local(i)), body)
          checkType(substitutedTerm, resultType, Γ.withLocalType(i, argumentType), i + 1)
        case FreeType(_) => throwError("type mismatch Lam/FreeType")
      }
    }

  private def substitute(i: Int, replacement: InferrableTerm, term: InferrableTerm): InferrableTerm = term match {
    case Ann(term, typ) => Ann(substitute(i, replacement, term), typ)
    case Bound(j) => if (i == j) replacement else Bound(j)
    case Free(name) => Free(name)
    case App(function, argument) => App(substitute(i, replacement, function), substitute(i, replacement, argument))
  }

  private def substitute(i: Int, replacement: InferrableTerm, term: CheckableTerm): CheckableTerm = term match {
    case Inf(term) => Inf(substitute(i, replacement, term))
    case Lam(body) => Lam(substitute(i + 1, replacement, body))
  }
}
