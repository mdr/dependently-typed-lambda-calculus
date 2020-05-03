package dependentlyTyped

case class InterpreterMonad[A](f: InterpreterState => (InterpreterState, A)) {

  def apply(state: InterpreterState = InterpreterState.initial): (InterpreterState, A) = f(state)

  def run(state: InterpreterState = InterpreterState.initial): A = apply(state)._2

  def map[B](g: A => B): InterpreterMonad[B] = InterpreterMonad(state => {
    val (newState, result) = apply(state)
    (newState, g(result))
  })

  def flatMap[B](g: A => InterpreterMonad[B]): InterpreterMonad[B] = InterpreterMonad(state => {
    val (newState, result) = f(state)
    g(result)(newState)
  })

}


object InterpreterState {

  val initial: InterpreterState = InterpreterState()

}


sealed trait InterpreterResult

object InterpreterResult {
  type InterpreterOutcome = Either[String, InterpreterResult]

  case class Evaluated(name: String, value: Value, typ: Type) extends InterpreterResult

  case class Assume(name: String, typ: Type) extends InterpreterResult

}


case class InterpreterState(letBindings: Map[String, Value] = Map.empty, assumptions: Map[String, Type] = Map.empty) {
  def merge(that: InterpreterState): InterpreterState = copy(letBindings = this.letBindings ++ that.letBindings, assumptions = this.assumptions ++ that.assumptions)

  val Γ: Context = assumptions.foldLeft(Context.empty) { case (context, (name, info)) => context.withGlobal(name, info) }

  val environment: Environment =
    letBindings.foldLeft(Environment.empty) { case (env, (name, value)) => env.extendWith(name, value) }

  def bind(name: String, value: Value, typ: Type): InterpreterState =
    copy(letBindings = letBindings + (name -> value), assumptions = assumptions + (name -> typ))

  def assume(name: String, typ: Type): InterpreterState = copy(assumptions = assumptions + (name -> typ))

}

object Interpreter {

  def interpret(s: String): InterpreterMonad[Either[String, InterpreterResult]] = InterpreterMonad(interpreterState => {
    val resultEither = interpret(s, interpreterState)

    val newInterpreterState = resultEither match {
      case Left(_) => interpreterState
      case Right(InterpreterResult.Evaluated(name, value, typ)) => interpreterState.bind(name, value, typ)
      case Right(InterpreterResult.Assume(name, info)) => interpreterState.assume(name, info)
    }
    newInterpreterState -> resultEither
  })

  def eval(name: String): InterpreterMonad[CheckableTerm] =
    for {
      resultEither <- interpret(name)
      Right(InterpreterResult.Evaluated(_, value, _)) = resultEither
    } yield Quoter.quote(value)

  private def interpret(s: String, state: InterpreterState): Either[String, InterpreterResult] = {
    for {
      statement <- Parser.parseStatementSafe(s)
      result <- interpretStatement(statement, state)
    } yield result
  }

  private def interpretStatement(statement: Statement, state: InterpreterState): Either[String, InterpreterResult] =
    statement match {
      case Statement.Eval(term) => interpretExpression("it", term, state)
      case Statement.Let(name, term) => interpretExpression(name, term, state)
      case Statement.Assume(name, typ) => interpretAssume(name, typ, state)
    }

  private def interpretAssume(name: String, typ: InferrableTerm, state: InterpreterState): Either[String, InterpreterResult] =
    for {
      _ <- TypeChecker.checkType(Term.Inf(typ), Value.*, state.Γ, 0)
    } yield InterpreterResult.Assume(name, Evaluator.eval(typ, state.environment))

  private def interpretExpression(name: String, term: InferrableTerm, state: InterpreterState): Either[String, InterpreterResult] =
    for {
      typ <- TypeChecker.inferType(term, state.Γ)
    } yield InterpreterResult.Evaluated(name, Evaluator.eval(term, state.environment), typ)

}
