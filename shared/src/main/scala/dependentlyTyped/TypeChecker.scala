package dependentlyTyped

import scala.annotation.tailrec
import Substitutions._

object TypeChecker {

  import Term._

  type Result[A] = Either[String, A]

  private def throwError(error: String) = Left(error)

  // TODO
  //  def checkKind(typ: Type, kind: Kind, Γ: Context): Result[Unit] =
  //    typ match {
  //      case FreeType(name) => Γ(name) match {
  //        case Some(HasKind(_)) => Right(())
  //        case Some(HasType(_)) => throwError(s"type $name has type instead of kind")
  //        case None => throwError(s"unknown identifier $name")
  //      }
  //      case FunctionType(functionType, argumentType) =>
  //        for {
  //          _ <- checkKind(functionType, *, Γ)
  //          _ <- checkKind(argumentType, *, Γ)
  //        } yield ()
  //    }

  def inferType(term: InferrableTerm, Γ: Context, bindersPassed: Int = 0): Result[Type] =
    term match {
      case Annotated(term, typ) =>
        for {
          _ <- checkType(typ, Value.*, Γ, bindersPassed)
          evaluatedType = Evaluator.eval(typ, Environment.empty)
          _ <- checkType(term, evaluatedType, Γ, bindersPassed)
        } yield evaluatedType
      case FreeVariable(name) =>
        Γ(name) match {
          case Some(typ) => Right(typ)
          case None => throwError(s"Unknown identifier: $name")
        }
      case Application(function, argument) =>
        for {
          functionType <- inferType(function, Γ, bindersPassed)
          resultType <- functionType match {
            case Value.Pi(argumentType, dependentResultType) =>
              for {
                _ <- checkType(argument, argumentType, Γ, bindersPassed)
              } yield dependentResultType(Evaluator.eval(argument, Environment.empty))
            case _ => throwError(s"Value of type ${functionType} is not a valid function")
          }
        } yield resultType
      case BoundVariable(_) => throwError("Unexpected Bound term in type checking")
      case Term.* => Right(Value.*)
      case Pi(argumentType, resultType) =>
        for {
          _ <- checkType(argumentType, Value.*, Γ, bindersPassed)
          evaluatedArgumentType = Evaluator.eval(argumentType, Environment.empty)
          freshName = Name.Local(bindersPassed)
          substitutedTerm = resultType.substitute(0, FreeVariable(freshName))
          _ <- checkType(substitutedTerm, Value.*, Γ.withLocalType(bindersPassed, evaluatedArgumentType), bindersPassed + 1)
        } yield Value.*
    }


  @tailrec
  def checkType(term: CheckableTerm, expectedType: Type, Γ: Context, bindersPassed: Int): Result[Unit] =
    term match {
      case Inf(term) =>
        for {
          inferredType <- inferType(term, Γ, bindersPassed)
          _ <- if (Quoter.quote(inferredType) == Quoter.quote(expectedType)) Right(()) else throwError(s"Type mismatch. Expected type '${Quoter.quote(expectedType)}', but was inferred as '${Quoter.quote(inferredType)}'.")
        } yield ()
      case Lambda(body) => expectedType match {
        case Value.Pi(argumentType, dependentResultType) =>
          val freshName = Name.Local(bindersPassed)
          val substitutedTerm = body.substitute(0, FreeVariable(freshName))
          checkType(substitutedTerm, dependentResultType(Value.freeVariable(Name.Local(bindersPassed))), Γ.withLocalType(bindersPassed, argumentType), bindersPassed + 1)
        case _ => throwError(s"type mismatch: a lambda ($term) cannot be $expectedType")
      }
    }

}