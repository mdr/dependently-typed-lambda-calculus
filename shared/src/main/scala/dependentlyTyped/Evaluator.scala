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
        val zeroValue = eval(zeroCase, env)
        val succValue = eval(succCase, env)

        def rec(value: Value): Value = value match {
          case Value.Zero => zeroValue
          case Value.Succ(subValue) => succValue(subValue)(rec(subValue))
          case Value.Neutral(neutral) => Value.Neutral(Neutral.NatElim(eval(motive, env), zeroValue, succValue, neutral))
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
      case Term.Fin(n) => Value.Fin(eval(n, env))
      case Term.FZero(n) => Value.FZero(eval(n, env))
      case Term.FSucc(n, term) => Value.FSucc(eval(n, env), eval(term, env))
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
      case Value.Fin(_) | Value.FZero(_) | Value.FSucc(_, _) | Value.Nil(_) | Value.Cons(_, _, _, _) | Value.Vec(_, _) | Value.Zero | Value.Succ(_) | Value.* | Value.Nat | Value.Pi(_, _) =>
        throw new AssertionError(s"Cannot apply $function as a function")
    }

}
