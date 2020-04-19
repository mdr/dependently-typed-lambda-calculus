package simplyTyped

case class Environment(bindings: Seq[Value] = Seq.empty) {

  def lookup(n: Int): Value = bindings(n)

  def extend(v: Value): Environment = Environment(v +: bindings)

}

object Evaluator {

  def eval(term: InferrableTerm, env: Environment = Environment()): Value =
    term match {
      case Ann(subterm, _) => eval(subterm, env)
      case Free(name) => Value.vfree(name)
      case Bound(n) => env.lookup(n)
      case App(function, argument) => vapp(eval(function, env), eval(argument, env))
    }

  private def eval(term: CheckableTerm, env: Environment): Value = term match {
    case Inf(subterm) => eval(subterm, env)
    case Lam(body) => LambdaValue(argument => eval(body, env.extend(argument)))
  }

  private def vapp(function: Value, argument: Value): Value =
    function match {
      case LambdaValue(function) => function(argument)
      case NeutralValue(value) => NeutralValue(AppNeutral(value, argument))
    }

}
