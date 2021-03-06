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
              } yield dependentResultType(Evaluator.eval(argument, environment))
            case _ => throwError(s"Value of type '$functionType' is not a valid function ('$function' applied to '$argument')")
          }
        } yield resultType
      case BoundVariable(_) => throwError("Unexpected Bound term in type checking")
      case Term.* => Right(Value.*)
      case Nat => Right(Value.*)
      case Zero => Right(Value.Nat)
      case Succ(term) =>
        for {
          _ <- checkType(term, Value.Nat, Γ, environment, bindersPassed)
        } yield Value.Nat
      case Term.NatElim(motive, zeroCase, succCase, n) =>
        for {
          // m :: Nat -> *
          _ <- checkType(motive, Value.Pi(Value.Nat, _ => Value.*), Γ, environment, bindersPassed)
          motiveValue = Evaluator.eval(motive, environment)

          // m :: Nat -> *
          _ <- checkType(zeroCase, Evaluator.apply(motiveValue, Value.Zero), Γ, environment, bindersPassed)

          // ∀ (l :: Nat) . m l -> m (Succ l)
          expectedSuccCaseType = Value.Pi(Value.Nat, l => Value.Pi(Evaluator.apply(motiveValue, l), _ => Evaluator.apply(motiveValue, Value.Succ(l))))
          _ <- checkType(succCase, expectedSuccCaseType, Γ, environment, bindersPassed)

          // k :: Nat
          _ <- checkType(n, Value.Nat, Γ, environment, bindersPassed)
          nValue = Evaluator.eval(n, environment)

          // m k
        } yield Evaluator.apply(motiveValue, nValue)
      case Nil(elementType) =>
        for {
          _ <- checkType(elementType, Value.*, Γ, environment, bindersPassed)
          evaluatedElementType = Evaluator.eval(elementType, environment)
        } yield Value.Vec(evaluatedElementType, Value.Zero)
      case Cons(elementType, length, head, tail) =>
        for {
          _ <- checkType(elementType, Value.*, Γ, environment, bindersPassed)
          _ <- checkType(length, Value.Nat, Γ, environment, bindersPassed)
          evaluatedElementType = Evaluator.eval(elementType, environment)
          evaluatedLength = Evaluator.eval(length, environment)
          _ <- checkType(head, evaluatedElementType, Γ, environment, bindersPassed)
          _ <- checkType(tail, Value.Vec(evaluatedElementType, evaluatedLength), Γ, environment, bindersPassed)
        } yield Value.Vec(evaluatedElementType, Value.Succ(evaluatedLength))
      case Vec(elementType, length) =>
        for {
          _ <- checkType(elementType, Value.*, Γ, environment, bindersPassed)
          _ <- checkType(length, Value.Nat, Γ, environment, bindersPassed)
        } yield Value.*
      case VecElim(elementType, motive, nilCase, consCase, length, vector) =>
        for {
          // a :: *
          _ <- checkType(elementType, Value.*, Γ, environment, bindersPassed)
          evaluatedElementType = Evaluator.eval(elementType, environment)

          // m :: (forall (k :: Nat) . Vec a k -> *)
          expectedMotiveType = Value.Pi(Value.Nat, length => Value.Pi(Value.Vec(evaluatedElementType, length), _ => Value.*))
          _ <- checkType(motive, expectedMotiveType, Γ, environment, bindersPassed)
          evaluatedMotive = Evaluator.eval(motive, environment)

          // m 0 (Nil a)
          expectedNilCaseType = evaluatedMotive(Value.Zero)(Value.Nil(evaluatedElementType))
          _ <- checkType(nilCase, expectedNilCaseType, Γ, environment, bindersPassed)

          // forall (l :: Nat) (x :: a) (xs :: Vec a l) . m l xs -> m (Succ l) (Cons a l x xs)
          expectedConsCaseType = Value.Pi(Value.Nat, l =>
            Value.Pi(evaluatedElementType, x =>
              Value.Pi(Value.Vec(evaluatedElementType, l), xs =>
                Value.Pi(evaluatedMotive(l)(xs), _ =>
                  evaluatedMotive(Value.Succ(l))(Value.Cons(evaluatedElementType, l, x, xs))))))
          _ <- checkType(consCase, expectedConsCaseType, Γ, environment, bindersPassed)

          // k :: Nat
          _ <- checkType(length, Value.Nat, Γ, environment, bindersPassed)
          evaluatedLength = Evaluator.eval(length, environment)

          // xs :: Vec a k
          _ <- checkType(vector, Value.Vec(evaluatedElementType, evaluatedLength), Γ, environment, bindersPassed)
          evaluatedVector = Evaluator.eval(vector, environment)

          //  m k xs
        } yield evaluatedMotive(evaluatedLength)(evaluatedVector)
      case Fin(n) =>
        for {
          _ <- checkType(n, Value.Nat, Γ, environment, bindersPassed)
        } yield Value.*
      case FZero(n) =>
        for {
          _ <- checkType(n, Value.Nat, Γ, environment, bindersPassed)
          evaluatedN = Evaluator.eval(n, environment)
        } yield Value.Fin(Value.Succ(evaluatedN))
      case FSucc(n, term) =>
        for {
          _ <- checkType(n, Value.Nat, Γ, environment, bindersPassed)
          evaluatedN = Evaluator.eval(n, environment)
          _ <- checkType(term, Value.Fin(evaluatedN), Γ, environment, bindersPassed)
        } yield Value.Fin(Value.Succ(evaluatedN))
      case FinElim(motive, zeroCase, succCase, n, fin) =>
        // m :: ∀ (n :: Nat) . Fin n -> *
        val expectedMotiveType = Value.Pi(Value.Nat, n => Value.Pi(Value.Fin(n), _ => Value.*))
        for {
          _ <- checkType(motive, expectedMotiveType, Γ, environment, bindersPassed)
          evaluatedMotive = Evaluator.eval(motive, environment)

          // ∀ n :: Nat . m (Succ n) (FZero n)
          expectedZeroCaseType = Value.Pi(Value.Nat, n => evaluatedMotive(Value.Succ(n))(Value.FZero(n)))
          _ <- checkType(zeroCase, expectedZeroCaseType, Γ, environment, bindersPassed)

          // ∀ (n :: Nat) (f :: Fin n) . m n f -> m (Succ n) (FSucc n f)
          expectedSuccCase = Value.Pi(Value.Nat, n => Value.Pi(Value.Fin(n), f => Value.Pi(evaluatedMotive(n)(f), _ => evaluatedMotive(Value.Succ(n))(Value.FSucc(n, f)))))
          _ <- checkType(succCase, expectedSuccCase, Γ, environment, bindersPassed)

          // n :: Nat
          _ <- checkType(n, Value.Nat, Γ, environment, bindersPassed)
          evaluatedN = Evaluator.eval(n, environment)

          // f :: Fin n
          _ <- checkType(fin, Value.Fin(evaluatedN), Γ, environment, bindersPassed)
          evaluatedFin = Evaluator.eval(fin, environment)

          // m n f
        } yield evaluatedMotive(evaluatedN)(evaluatedFin)
      case Pi(argumentType, resultType) =>
        for {
          _ <- checkType(argumentType, Value.*, Γ, environment, bindersPassed)
          evaluatedArgumentType = Evaluator.eval(argumentType, environment)
          freshName = Name.Local(bindersPassed)
          substitutedTerm = resultType.substitute(0, FreeVariable(freshName))
          _ <- checkType(substitutedTerm, Value.*, Γ.withLocalType(bindersPassed, evaluatedArgumentType), environment, bindersPassed + 1)
        } yield Value.*
      case Eq(typ, left, right) =>
        for {
          _ <- checkType(typ, Value.*, Γ, environment, bindersPassed)
          evaluatedType = Evaluator.eval(typ, environment)
          _ <- checkType(left, evaluatedType, Γ, environment, bindersPassed)
          _ <- checkType(right, evaluatedType, Γ, environment, bindersPassed)
        } yield Value.*
      case Refl(typ, value) =>
        for {
          _ <- checkType(typ, Value.*, Γ, environment, bindersPassed)
          evaluatedType = Evaluator.eval(typ, environment)
          _ <- checkType(value, evaluatedType, Γ, environment, bindersPassed)
          evaluatedValue = Evaluator.eval(value, environment)
        } yield Value.Eq(evaluatedType, evaluatedValue, evaluatedValue)
      case EqElim(typ, motive, reflCase, left, right, equality) =>
        for {
          // a :: *
          _ <- checkType(typ, Value.*, Γ, environment, bindersPassed)
          evaluatedType = Evaluator.eval(typ, environment)

          // m :: ∀ (l :: a) (r :: a) . (Eq a l r) -> *
          expectedMotiveType = Value.Pi(evaluatedType, l => Value.Pi(evaluatedType, r => Value.Pi(Value.Eq(evaluatedType, l, r), _ => Value.*)))
          _ <- checkType(motive, expectedMotiveType, Γ, environment, bindersPassed)
          evaluatedMotive = Evaluator.eval(motive, environment)

          // ∀ v :: a . m v v (Refl a v)
          expectedReflCaseType = Value.Pi(evaluatedType, v => evaluatedMotive(v)(v)(Value.Refl(evaluatedType, v)))
          _ <- checkType(reflCase, expectedReflCaseType, Γ, environment, bindersPassed)

          // l :: a
          _ <- checkType(left, evaluatedType, Γ, environment, bindersPassed)
          evaluatedLeft = Evaluator.eval(left, environment)

          // r :: a
          _ <- checkType(right, evaluatedType, Γ, environment, bindersPassed)
          evaluatedRight = Evaluator.eval(right, environment)

          // e :: Eq a l r
          _ <- checkType(equality, Value.Eq(evaluatedType, evaluatedLeft, evaluatedRight), Γ, environment, bindersPassed)
          evaluatedEquality = Evaluator.eval(equality, environment)

          // m l r e
        } yield evaluatedMotive(evaluatedLeft)(evaluatedRight)(evaluatedEquality)
    }

  @tailrec
  def checkType(term: CheckableTerm, expectedType: Type, Γ: Context, environment: Environment, bindersPassed: Int): Result[Unit] =
    term match {
      case Inf(term) =>
        for {
          inferredType <- inferType(term, Γ, environment, bindersPassed)
          _ <- if (Quoter.quote(inferredType) == Quoter.quote(expectedType)) Right(()) else throwError(s"Type mismatch for '$term'. Expected type '${Quoter.quote(expectedType)}', but was inferred as '${Quoter.quote(inferredType)}'.")
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
