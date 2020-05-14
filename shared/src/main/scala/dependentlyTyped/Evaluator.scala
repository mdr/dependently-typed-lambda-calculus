package dependentlyTyped

object Environment {

  val empty: Environment = Environment()

}

case class Environment(bindings: Seq[Value] = Seq.empty, namedValues: Map[String, Value] = Map.empty) {

  def apply(n: Int): Option[Value] = bindings.lift(n)

  def apply(name: String): Option[Value] = namedValues.get(name)

  def extendWith(value: Value): Environment = copy(bindings = value +: bindings)

  def extendWith(name: String, value: Value): Environment = copy(namedValues = namedValues + (name -> value))

}

object Evaluator {

  def eval(term: InferrableTerm, env: Environment = Environment.empty): Value =
    term match {
      case Term.Annotated(subterm, _) => eval(subterm, env)
      case Term.FreeVariable(Name.Global(name)) => env(name) getOrElse Value.freeVariable(Name.Global(name))
      case Term.FreeVariable(name) => Value.freeVariable(name)
      case Term.BoundVariable(n) => env(n) getOrElse (throw new AssertionError(s"Could not find binding for variable $n"))
      case Term.Application(function, argument) => eval(function, env)(eval(argument, env))
      case Term.* => Value.*
      case Term.Pi(argumentType, resultType) => Value.Pi(eval(argumentType, env), value => eval(resultType, env.extendWith(value)))
      case Term.Nat => Value.Nat
      case Term.Zero => Value.Zero
      case Term.Succ(subterm) => Value.Succ(eval(subterm, env))
      case Term.NatElim(motive, zeroCase, succCase, n) =>
        val evaluatedZeroCase = eval(zeroCase, env)
        val evaluatedSuccCase = eval(succCase, env)

        def rec(value: Value): Value = value match {
          case Value.Zero => evaluatedZeroCase
          case Value.Succ(subValue) => evaluatedSuccCase(subValue)(rec(subValue))
          case Value.Neutral(neutral) => Value.Neutral(Neutral.NatElim(eval(motive, env), evaluatedZeroCase, evaluatedSuccCase, neutral))
          case _ => throw new AssertionError(s"NatElim error: $value")
        }

        rec(eval(n, env))
      case Term.Nil(elementType) => Value.Nil(eval(elementType, env))
      case Term.Cons(elementType, length, head, tail) => Value.Cons(eval(elementType, env), eval(length, env), eval(head, env), eval(tail, env))
      case Term.Vec(elementType, length) => Value.Vec(eval(elementType, env), eval(length, env))
      case Term.VecElim(elementType, motive, nilCase, consCase, length, vector) =>
        val evaluatedNilCase = eval(nilCase, env)
        val evaluatedConsCase = eval(consCase, env)

        def rec(length: Value, vector: Value): Value = vector match {
          case Value.Nil(_) => evaluatedNilCase
          case Value.Cons(_, length, head, tail) => evaluatedConsCase(length)(head)(tail)(rec(length, tail))
          case Value.Neutral(neutral) => Value.Neutral(Neutral.VecElim(eval(elementType, env), eval(motive, env), evaluatedNilCase, evaluatedConsCase, length, neutral))
          case _ => throw new AssertionError(s"VecElim error: $vector")
        }

        rec(eval(length, env), eval(vector, env))
      case Term.FinElim(motive, zeroCase, succCase, n, fin) =>
        val evaluatedZeroCase = eval(zeroCase, env)
        val evaluatedSuccCase = eval(succCase, env)

        def rec(n: Value, fin: Value): Value = fin match {
          case Value.FZero(n) => evaluatedZeroCase(n)
          case Value.FSucc(n, value) => evaluatedSuccCase(n)(value)(rec(n, value))
          case Value.Neutral(neutral) => Value.Neutral(Neutral.FinElim(eval(motive, env), evaluatedZeroCase, evaluatedSuccCase, n, neutral))
          case _ => throw new AssertionError(s"FinElim error: $fin")
        }

        rec(eval(n, env), eval(fin, env))
      case Term.Fin(n) => Value.Fin(eval(n, env))
      case Term.FZero(n) => Value.FZero(eval(n, env))
      case Term.FSucc(n, term) => Value.FSucc(eval(n, env), eval(term, env))
      case Term.Eq(typ, left, right) => Value.Eq(eval(typ, env), eval(left, env), eval(right, env))
      case Term.Refl(typ, value) => Value.Refl(eval(typ, env), eval(value, env))
      case Term.EqElim(typ, motive, reflCase, left, right, equality) =>
        val evaluatedReflCase = eval(reflCase, env)
        eval(equality, env) match {
          case Value.Refl(_, z) => evaluatedReflCase(z)
          case Value.Neutral(neutral) => Value.Neutral(Neutral.EqElim(eval(typ, env), eval(motive, env), evaluatedReflCase, eval(left, env), eval(right, env), neutral))
          case v => throw new AssertionError(s"EqElim error: $v")
        }
    }

  def eval(term: CheckableTerm, env: Environment): Value =
    term match {
      case Term.Inf(subterm) => eval(subterm, env)
      case Term.Lambda(body) => Value.Lambda(argument => eval(body, env.extendWith(argument)))
    }

  def apply(function: Value, argument: Value): Value =
    function match {
      case Value.Lambda(function) => function(argument)
      case Value.Neutral(value) => Value.Neutral(Neutral.Application(value, argument))
      case Value.Eq(_, _, _) | Value.Refl(_, _) | Value.Fin(_) | Value.FZero(_) | Value.FSucc(_, _) |
           Value.Nil(_) | Value.Cons(_, _, _, _) | Value.Vec(_, _) | Value.Zero | Value.Succ(_) | Value.* |
           Value.Nat | Value.Pi(_, _) =>
        throw new AssertionError(s"Cannot apply $function as a function")
    }

}
