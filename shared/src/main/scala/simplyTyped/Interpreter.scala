package simplyTyped

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


sealed trait InterpreterResult {
  val newInterpreterState: InterpreterState
}

object InterpreterResult {

  case class Assumption(name: String, info: Info)

  type InterpreterOutcome = Either[String, InterpreterResult]

  case class Evaluated(name: String, value: Value, typ: Type, newInterpreterState: InterpreterState) extends InterpreterResult

  case class Assume(assumptions: Seq[Assumption], newInterpreterState: InterpreterState) extends InterpreterResult

}


case class InterpreterState(letBindings: Map[String, Value] = Map.empty, assumptions: Map[String, Info] = Map.empty) {
  def merge(that: InterpreterState): InterpreterState = copy(letBindings = this.letBindings ++ that.letBindings, assumptions = this.assumptions ++ that.assumptions)

  val Γ: Context = assumptions.foldLeft(Context.empty) { case (context, (name, info)) => context.withGlobal(name, info) }

  val environment: Environment =
    letBindings.foldLeft(Environment.empty) { case (env, (name, value)) => env.extendWith(name, value) }

  def bind(name: String, value: Value, typ: Type): InterpreterState =
    copy(letBindings = letBindings + (name -> value), assumptions = assumptions + (name -> HasType(typ)))

  def assume(name: String, info: Info): InterpreterState = copy(assumptions = assumptions + (name -> info))

  def assume(name: String, kind: Kind): InterpreterState = assume(name, HasKind(kind))

  def assume(name: String, typ: Type): InterpreterState = assume(name, HasType(typ))

}

object Interpreter {

  def interpret(s: String): InterpreterMonad[Either[String, InterpreterResult]] = InterpreterMonad(interpreterState => {
    val resultEither = interpret(s, interpreterState)

    val newInterpreterState = resultEither match {
      case Left(_) => interpreterState
      case Right(result) => result.newInterpreterState
    }
    newInterpreterState -> resultEither
  })

  def eval(name: String): InterpreterMonad[CheckableTerm] =
    for {
      resultEither <- interpret(name)
      Right(InterpreterResult.Evaluated(_, value, _, _)) = resultEither
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
      case Statement.Assume(assumptions) => interpretAssumptions(assumptions, state)
    }

  private def interpretAssumptions(assumptions: Seq[Assumption], state: InterpreterState): Either[String, InterpreterResult] = {
    var currentState = state
    var assumptionResults: Seq[InterpreterResult.Assumption] = Seq.empty
    for (Assumption(name, info) <- assumptions) {
      interpretAssumption(name, info, currentState) match {
        case Left(message) => return Left(message)
        case Right((assumptionResult, newState)) =>
          currentState = newState
          assumptionResults :+= assumptionResult
      }
    }
    Right(InterpreterResult.Assume(assumptionResults, currentState))
  }

  private def interpretAssumption(name: String, info: Info, state: InterpreterState): Either[String, (InterpreterResult.Assumption, InterpreterState)] =
    info match {
      case HasKind(_) => Right(InterpreterResult.Assumption(name, info) -> state.assume(name, info))
      case HasType(typ) => interpretAssumption(name, typ, state)
    }

  private def interpretAssumption(name: String, typ: Type, state: InterpreterState): Either[String, (InterpreterResult.Assumption, InterpreterState)] =
    for {
      _ <- TypeChecker.checkKind(typ, *, state.Γ)
    } yield InterpreterResult.Assumption(name, HasType(typ)) -> state.assume(name, HasType(typ))

  private def interpretExpression(name: String, term: InferrableTerm, state: InterpreterState): Either[String, InterpreterResult] =
    for {
      typ <- TypeChecker.inferType(term, state.Γ)
      value = Evaluator.eval(term, state.environment)
    } yield InterpreterResult.Evaluated(name, value, typ, state.bind(name, value, typ))

}
