package dependentlyTyped

import scala.annotation.tailrec
import Substitutions._

object TypeChecker {

  import Term._

  type Result[A] = Either[String, A]

  private def throwError(error: String) = Left(error)

  def inferType(term: InferrableTerm, Γ: Context, environment: Environment, bindersPassed: Int = 0): Result[Type] =
    term match {
      case Annotated(term, typ) =>
        for {
          _ <- checkType(typ, Value.*, Γ, environment, bindersPassed)
          evaluatedType = Evaluator.eval(typ, environment)
          _ <- checkType(term, evaluatedType, Γ, environment, bindersPassed)
        } yield evaluatedType
      case FreeVariable(name) =>
        Γ(name) match {
          case Some(typ) => Right(typ)
          case None => throwError(s"Unknown identifier: $name")
        }
      case Application(function, argument) =>
        for {
          functionType <- inferType(function, Γ, environment, bindersPassed)
          resultType <- functionType match {
            case Value.Pi(argumentType, dependentResultType) =>
              for {
                _ <- checkType(argument, argumentType, Γ, environment, bindersPassed)
              } yield dependentResultType(Evaluator.eval(argument, Environment.empty))
            case _ => throwError(s"Value of type $functionType is not a valid function")
          }
        } yield resultType
      case BoundVariable(_) => throwError("Unexpected Bound term in type checking")
      case Term.* => Right(Value.*)
      case Term.Nat => Right(Value.*)
      case Term.Zero => Right(Value.Nat)
      case Term.Succ(term) =>
        for {
          _ <- checkType(term, Value.Nat, Γ, environment, bindersPassed)
        } yield Value.Nat
      case Term.NatElim(motive, zeroCase, succCase, n) =>
        for {
          _ <- checkType(motive, Value.Pi(Value.Nat, _ => Value.*), Γ, environment, bindersPassed)
          motiveValue = Evaluator.eval(motive, environment)
          _ <- checkType(zeroCase, Evaluator.apply(motiveValue, Value.Zero), Γ, environment, bindersPassed)
          expectedSuccCaseType = Value.Pi(Value.Nat, l => Value.Pi(Evaluator.apply(motiveValue, l), _ => Evaluator.apply(motiveValue, Value.Succ(l))))
          _ <- checkType(succCase, expectedSuccCaseType, Γ, environment, bindersPassed)
          _ <- checkType(n, Value.Nat, Γ, environment, bindersPassed)
          nValue = Evaluator.eval(n, environment)
        } yield Evaluator.apply(motiveValue, nValue)
      case Term.Nil(elementType) =>
        for {
          _ <- checkType(elementType, Value.*, Γ, environment, bindersPassed)
          evaluatedElementType = Evaluator.eval(elementType, Environment.empty)
        } yield Value.Vec(evaluatedElementType, Value.Zero)
      case Term.Cons(elementType, length, head, tail) =>
        for {
          _ <- checkType(elementType, Value.*, Γ, environment, bindersPassed)
          _ <- checkType(length, Value.Nat, Γ, environment, bindersPassed)
          evaluatedElementType = Evaluator.eval(elementType, Environment.empty)
          evaluatedLength = Evaluator.eval(length, Environment.empty)
          _ <- checkType(head, evaluatedElementType, Γ, environment, bindersPassed)
          _ <- checkType(tail, Value.Vec(evaluatedElementType, evaluatedLength), Γ, environment, bindersPassed)
        } yield Value.Vec(evaluatedElementType, Value.Succ(evaluatedLength))
      case Term.Vec(elementType, length) =>
        for {
          _ <- checkType(elementType, Value.*, Γ, environment, bindersPassed)
          _ <- checkType(length, Value.Nat, Γ, environment, bindersPassed)
        } yield Value.*
      case Pi(argumentType, resultType) =>
        for {
          _ <- checkType(argumentType, Value.*, Γ, environment, bindersPassed)
          evaluatedArgumentType = Evaluator.eval(argumentType, Environment.empty)
          freshName = Name.Local(bindersPassed)
          substitutedTerm = resultType.substitute(0, FreeVariable(freshName))
          _ <- checkType(substitutedTerm, Value.*, Γ.withLocalType(bindersPassed, evaluatedArgumentType), environment, bindersPassed + 1)
        } yield Value.*
    }


  @tailrec
  def checkType(term: CheckableTerm, expectedType: Type, Γ: Context, environment: Environment, bindersPassed: Int): Result[Unit] =
    term match {
      case Inf(term) =>
        for {
          inferredType <- inferType(term, Γ, environment, bindersPassed)
          _ <- if (Quoter.quote(inferredType) == Quoter.quote(expectedType)) Right(()) else throwError(s"Type mismatch. Expected type '${Quoter.quote(expectedType)}', but was inferred as '${Quoter.quote(inferredType)}'.")
        } yield ()
      case Lambda(body) => expectedType match {
        case Value.Pi(argumentType, dependentResultType) =>
          val freshName = Name.Local(bindersPassed)
          val substitutedTerm = body.substitute(0, FreeVariable(freshName))
          checkType(substitutedTerm, dependentResultType(Value.freeVariable(Name.Local(bindersPassed))), Γ.withLocalType(bindersPassed, argumentType), environment, bindersPassed + 1)
        case _ => throwError(s"type mismatch: a lambda ($term) cannot be $expectedType")
      }
    }

}
