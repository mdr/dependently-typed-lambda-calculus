package simplyTyped

import scala.annotation.tailrec
import simplyTyped.Substitutions._

object TypeChecker {

  import Term._

  type Result[A] = Either[String, A]

  private def throwError(error: String) = Left(error)

  def checkKind(typ: Type, kind: Kind, Γ: Context): Result[Unit] =
    typ match {
      case FreeType(name) => Γ(name) match {
        case Some(HasKind(_)) => Right(())
        case Some(HasType(_)) => throwError(s"type $name has type instead of kind")
        case None => throwError(s"unknown identifier $name")
      }
      case FunctionType(functionType, argumentType) =>
        for {
          _ <- checkKind(functionType, *, Γ)
          _ <- checkKind(argumentType, *, Γ)
        } yield ()
    }

  def inferType(term: InferrableTerm, Γ: Context, bindersPassed: Int = 0): Result[Type] =
    term match {
      case Annotated(term, typ) =>
        for {
          _ <- checkKind(typ, *, Γ)
          _ <- checkType(term, typ, Γ, bindersPassed)
        } yield typ
      case FreeVariable(name) =>
        Γ(name) match {
          case Some(HasType(typ)) => Right(typ)
          case Some(HasKind(_)) => throwError(s"$name unexpectedly has a kind, not a type")
          case None => throwError(s"Unknown identifier: $name")
        }
      case Application(function, argument) =>
        for {
          functionType <- inferType(function, Γ, bindersPassed)
          resultType <- functionType match {
            case FunctionType(argumentType, resultType) =>
              for {
                _ <- checkType(argument, argumentType, Γ, bindersPassed)
              } yield resultType
            case FreeType(_) => throwError(s"Value of type ${functionType} is not a valid function")
          }
        } yield resultType
      case BoundVariable(_) => throwError("Unexpected Bound term in type checking")
    }


  @tailrec
  def checkType(term: CheckableTerm, expectedType: Type, Γ: Context, bindersPassed: Int): Result[Unit] =
    term match {
      case Inf(term) =>
        for {
          inferredType <- inferType(term, Γ, bindersPassed)
          _ <- if (inferredType == expectedType) Right(()) else throwError(s"Type mismatch. Expected type '$expectedType', but was inferred as '$inferredType'.")
        } yield ()
      case Lambda(body) => expectedType match {
        case FunctionType(argumentType, resultType) =>
          val freshName = Name.Local(bindersPassed)
          val substitutedTerm = body.substitute(0, FreeVariable(freshName))
          checkType(substitutedTerm, resultType, Γ.withLocalType(bindersPassed, argumentType), bindersPassed + 1)
        case FreeType(_) => throwError("type mismatch Lam/FreeType")
      }
    }

}
