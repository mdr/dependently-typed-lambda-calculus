package simplyTyped

import simplyTyped.Name.Global

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
      case Term.FreeVariable(Global(name)) => env(name) getOrElse Value.freeVariable(Global(name))
      case Term.FreeVariable(name) => Value.freeVariable(name)
      case Term.BoundVariable(n) => env(n) getOrElse (throw new AssertionError(s"Could not find binding for variable $n"))
      case Term.Application(function, argument) => apply(eval(function, env), eval(argument, env))
    }

  private def eval(term: CheckableTerm, env: Environment): Value =
    term match {
      case Term.Inf(subterm) => eval(subterm, env)
      case Term.Lambda(body) => Value.Lambda(argument => eval(body, env.extendWith(argument)))
    }

  private def apply(function: Value, argument: Value): Value =
    function match {
      case Value.Lambda(function) => function(argument)
      case Value.Neutral(value) => Value.Neutral(Neutral.Application(value, argument))
    }

}
