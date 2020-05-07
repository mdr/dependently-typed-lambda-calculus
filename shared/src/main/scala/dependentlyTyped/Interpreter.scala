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

sealed trait InterpreterResult {
  val newInterpreterState: InterpreterState
}

object InterpreterResult {

  case class Assumption(name: String, typ: Type)

  type InterpreterOutcome = Either[String, InterpreterResult]

  case class Evaluated(name: String, value: Value, typ: Type, newInterpreterState: InterpreterState) extends InterpreterResult

  case class Assume(assumptions: Seq[Assumption], newInterpreterState: InterpreterState) extends InterpreterResult

}

object InterpreterState {

  val empty: InterpreterState = InterpreterState()

  val initial: InterpreterState = empty
    .interpret("let Succ = (λn -> Succ_ n) :: Nat -> Nat")
    .interpret("let natElim = (λm mz ms k -> natElim_ m mz ms k) :: ∀ (m :: Nat -> *) . m 0 -> (∀ (l :: Nat) . m l -> m (Succ_ l)) -> ∀ (k :: Nat) . m k")
    .interpret("let Nil = (λa -> Nil_ a) :: ∀ (a :: *) . Vec_ a 0")
    .interpret("let Cons = (λa n h t -> Cons_ a n h t) :: ∀ (a :: *) (n :: Nat) . a -> Vec_ a n -> Vec_ a (Succ n)")
    .interpret("let Vec = (λa n -> Vec_ a n) :: * -> Nat -> *")

}

case class InterpreterState(letBindings: Map[String, Value] = Map.empty, assumptions: Map[String, Type] = Map.empty) {

  def merge(that: InterpreterState): InterpreterState = copy(letBindings = this.letBindings ++ that.letBindings, assumptions = this.assumptions ++ that.assumptions)

  val Γ: Context = assumptions.foldLeft(Context.empty) { case (context, (name, info)) => context.withGlobal(name, info) }

  val environment: Environment =
    letBindings.foldLeft(Environment.empty) { case (env, (name, value)) => env.extendWith(name, value) }

  def bind(name: String, value: Value, typ: Type): InterpreterState =
    copy(letBindings = letBindings + (name -> value), assumptions = assumptions + (name -> typ))

  def assume(name: String, typ: Type): InterpreterState = copy(assumptions = assumptions + (name -> typ))

  def interpret(statement: String): InterpreterState = {
    val (newState, Right(_)) = Interpreter.interpret(statement)(this)
    newState
  }
}

object Interpreter {

  def interpret(s: String): InterpreterMonad[Either[String, InterpreterResult]] = InterpreterMonad(interpreterState => {
    val resultEither = interpret(s, interpreterState)

    try {
      val newInterpreterState = resultEither match {
        case Left(_) => interpreterState
        case Right(result) => result.newInterpreterState
      }
      newInterpreterState -> resultEither
    } catch {
      case e: Throwable => interpreterState -> Left(s"Error: ${e.getMessage}")
    }
  })

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
    for (Assumption(name, typ) <- assumptions) {
      interpretAssumption(name, typ, currentState) match {
        case Left(message) => return Left(message)
        case Right((assumptionResult, newState)) =>
          currentState = newState
          assumptionResults :+= assumptionResult
      }
    }
    Right(InterpreterResult.Assume(assumptionResults, currentState))
  }

  private def interpretAssumption(name: String, typ: InferrableTerm, state: InterpreterState): Either[String, (InterpreterResult.Assumption, InterpreterState)] =
    for {
      _ <- TypeChecker.checkType(Term.Inf(typ), Value.*, state.Γ, state.environment, 0)
      evaluatedType = Evaluator.eval(typ, state.environment)
    } yield InterpreterResult.Assumption(name, evaluatedType) -> state.assume(name, evaluatedType)

  private def interpretExpression(name: String, term: InferrableTerm, state: InterpreterState): Either[String, InterpreterResult] =
    for {
      typ <- TypeChecker.inferType(term, state.Γ, state.environment)
      value = Evaluator.eval(term, state.environment)
    } yield InterpreterResult.Evaluated(name, value, typ, state.bind(name, value, typ))

}
