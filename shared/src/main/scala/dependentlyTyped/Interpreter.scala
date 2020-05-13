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
    .interpret("let Succ = (λn -> Succ n) :: Nat -> Nat")
    .interpret("let natElim = (λm mz ms k -> natElim m mz ms k) :: ∀ (m :: Nat -> *) . m 0 -> (∀ (l :: Nat) . m l -> m (Succ l)) -> ∀ (k :: Nat) . m k")
    .interpret("let Nil = (λa -> Nil a) :: ∀ (a :: *) . Vec a 0")
    .interpret("let Cons = (λa n h t -> Cons a n h t) :: ∀ (a :: *) (n :: Nat) . a -> Vec a n -> Vec a (Succ n)")
    .interpret("let Vec = (λa n -> Vec a n) :: * -> Nat -> *")
    .interpret(
      """let vecElim = (λa m mn mc k vs -> vecElim a m mn mc k vs) ::
        | ∀ (a :: *) (m :: (forall (k :: Nat) . Vec a k -> *)) . m 0 (Nil a)
        | -> (forall (l :: Nat) (x :: a) (xs :: Vec a l) . m l xs -> m (Succ l) (Cons a l x xs))
        | -> forall (k :: Nat) (xs :: Vec a k) . m k xs """.stripMargin)
    .interpret("let Fin = (λn -> Fin n) :: Nat -> *")
    .interpret("let FZero = (λn -> FZero n) :: ∀ (n :: Nat) . Fin (Succ n)")
    .interpret("let FSucc = (λn f -> FSucc n f) :: ∀ (n :: Nat) . Fin n -> Fin (Succ n)")
    .interpret("let finElim = (λm mz ms k f -> finElim m mz ms k f) :: ∀ (m :: ∀ (n :: Nat) . Fin n -> *) . (∀ n :: Nat . m (Succ n) (FZero n)) -> (∀ (n :: Nat) (f :: Fin n) . m n f -> m (Succ n) (FSucc n f)) -> ∀ (n :: Nat) (f :: Fin n) . m n f")

  val prelude: InterpreterState = initial
    .interpret("""let id = (\ a x -> x) :: forall (a :: *) . a -> a""")
    .interpret("""let const = (\ a b x y -> x) :: forall (a :: *) (b :: *) . a -> b -> a""")
    .interpret(
      """let plus =
        |  natElim
        |    ( \ _ -> Nat -> Nat )
        |    ( \ n -> n )
        |    ( \ p rec n -> Succ (rec n) ) """.stripMargin)
    .interpret(
      """let pred =
        |  natElim
        |    ( \ _ -> Nat )
        |    Zero
        |    ( \ n' _rec -> n' )""".stripMargin)
    .interpret(
      """let natFold =
        |  ( \ m mz ms -> natElim
        |                   ( \ _ -> m )
        |                   mz
        |                   ( \ n' rec -> ms rec ) )
        |  :: forall (m :: *) . m -> (m -> m) -> Nat -> m""".stripMargin)
    .interpret(
      """let nat1Elim =
        |  ( \ m m0 m1 ms -> natElim m m0
        |                            (\ p rec -> natElim (\ n -> m (Succ n)) m1 ms p) )
        |  :: forall (m :: Nat -> *) . m 0 -> m 1 ->
        |     (forall n :: Nat . m (Succ n) -> m (Succ (Succ n))) ->
        |     forall (n :: Nat) . m n""".stripMargin)
    .interpret(
      """let nat2Elim =
        |  ( \ m m0 m1 m2 ms -> nat1Elim m m0 m1
        |                                (\ p rec -> natElim (\ n -> m (Succ (Succ n))) m2 ms p) )
        |  :: forall (m :: Nat -> *) . m 0 -> m 1 -> m 2 ->
        |     (forall n :: Nat . m (Succ (Succ n)) -> m (Succ (Succ (Succ n)))) ->
        |     forall (n :: Nat) . m n""".stripMargin)
    .interpret("""let inc = natFold Nat (Succ Zero) Succ""")
    .interpret(
      """let replicate =
        |  ( natElim
        |      ( \ n -> forall (a :: *) . a -> Vec a n )
        |      ( \ a _ -> Nil a )
        |      ( \ n' rec_n' a x -> Cons a n' x (rec_n' a x) ) )
        |  :: forall (n :: Nat) . forall (a :: *) . a -> Vec a n""".stripMargin)
    .interpret(
      """let replicate' =
        |  (\ a n x -> natElim (Vec a)
        |                      (Nil a)
        |                      (\ n' rec_n' -> Cons a n' x rec_n') n)
        |  :: forall (a :: *) (n :: Nat) . a -> Vec a n""".stripMargin)
    .interpret(
      """let fromto =
        |  natElim
        |    ( \ n -> Vec Nat n )
        |    ( Nil Nat )
        |    ( \ n' rec_n' -> Cons Nat n' n' rec_n' )""".stripMargin)
    .interpret(
      """let append =
        |  ( \ a -> vecElim a
        |             (\ m _ -> forall (n :: Nat) . Vec a n -> Vec a (plus m n))
        |             (\ _ v -> v)
        |             (\ m v vs rec n w -> Cons a (plus m n) v (rec n w)))
        |  ::  forall (a :: *) (m :: Nat) (v :: Vec a m) (n :: Nat) (w :: Vec a n).
        |      Vec a (plus m n)
        |""".stripMargin)
    .interpret(
      """let finNat = finElim (\ _ _ -> Nat)
        |                     (\ _ -> Zero)
        |                     (\ _ _ rec -> Succ rec)""".stripMargin)
    .interpret("""let Unit = Fin 1""")
    .interpret("""let U = FZero 0""".stripMargin)
    .interpret(
      """let unitElim =
        |  ( \ m mu -> finElim ( nat1Elim (\ n -> Fin n -> *)
        |                                 (\ _ -> Unit)
        |                                 (\ x -> m x)
        |                                 (\ _ _ _ -> Unit) )
        |                      ( natElim (\ n -> natElim (\ n -> Fin (Succ n) -> *)
        |                                                (\ x -> m x)
        |                                                (\ _ _ _ -> Unit)
        |                                                n (FZero n))
        |                                mu
        |                                (\ _ _ -> U) )
        |                      ( \ n f _ -> finElim (\ n f -> natElim (\ n -> Fin (Succ n) -> *)
        |                                                             (\ x -> m x)
        |                                                             (\ _ _ _ -> Unit)
        |                                                             n (FSucc n f))
        |                                           (\ _ -> U)
        |                                           (\ _ _ _ -> U)
        |                                           n f )
        |                      1 )
        |  :: forall (m :: Unit -> *) . m U -> forall (u :: Unit) . m u
        |""".stripMargin)
    .interpret("""let Void = Fin 0""")
    .interpret(
      """let voidElim =
        |  ( \ m -> finElim (natElim (\ n -> Fin n -> *)
        |                            (\ x -> m x)
        |                            (\ _ _ _ -> Unit))
        |                   (\ _ -> U)
        |                   (\ _ _ _ -> U)
        |                   0 )
        |  :: forall (m :: Void -> *) (v :: Void) . m v""".stripMargin)
    .interpret("""let Bool = Fin 2""")
    .interpret("""let False = FZero 1""")
    .interpret("""let True  = FSucc 1 (FZero 0)""")
    .interpret(
      """let boolElim =
        |  ( \ m mf mt -> finElim ( nat2Elim (\ n -> Fin n -> *)
        |                                    (\ _ -> Unit) (\ _ -> Unit)
        |                                    (\ x -> m x)
        |                                    (\ _ _ _ -> Unit) )
        |                         ( nat1Elim ( \ n -> nat1Elim (\ n -> Fin (Succ n) -> *)
        |                                                      (\ _ -> Unit)
        |                                                      (\ x -> m x)
        |                                                      (\ _ _ _ -> Unit)
        |                                                      n (FZero n))
        |                                    U mf (\ _ _ -> U) )
        |                         ( \ n f _ -> finElim ( \ n f -> nat1Elim (\ n -> Fin (Succ n) -> *)
        |                                                                  (\ _ -> Unit)
        |                                                                  (\ x -> m x)
        |                                                                  (\ _ _ _ -> Unit)
        |                                                                  n (FSucc n f) )
        |                                              ( natElim
        |                                                  ( \ n -> natElim
        |                                                             (\ n -> Fin (Succ (Succ n)) -> *)
        |                                                             (\ x -> m x)
        |                                                             (\ _ _ _ -> Unit)
        |                                                             n (FSucc (Succ n) (FZero n)) )
        |                                                  mt (\ _ _ -> U) )
        |                                              ( \ n f _ -> finElim
        |                                                             (\ n f -> natElim
        |                                                                         (\ n -> Fin (Succ (Succ n)) -> *)
        |                                                                         (\ x -> m x)
        |                                                                         (\ _ _ _ -> Unit)
        |                                                                         n (FSucc (Succ n) (FSucc n f)))
        |                                                             (\ _ -> U)
        |                                                             (\ _ _ _ -> U)
        |                                                             n f )
        |                                              n f )
        |                         2 )
        |  :: forall (m :: Bool -> *) . m False -> m True -> forall (b :: Bool) . m b""".stripMargin)
    .interpret("""let not = boolElim (\ _ -> Bool) True False""")
    .interpret("""let and = boolElim (\ _ -> Bool -> Bool) (\ _ -> False) (id Bool)""")
    .interpret("""let or  = boolElim (\ _ -> Bool -> Bool) (id Bool) (\ _ -> True)""")
    .interpret("""let iff = boolElim (\ _ -> Bool -> Bool) not (id Bool)""")
    .interpret("""let xor = boolElim (\ _ -> Bool -> Bool) (id Bool) not""")
    .interpret("""let even    = natFold Bool True not""")
    .interpret("""let odd     = natFold Bool False not""")
    .interpret("""let isZero  = natFold Bool True (\ _ -> False)""")
    .interpret("""let isSucc  = natFold Bool False (\ _ -> True)""")
    .interpret(
      """let natEq =
        |  natElim
        |    ( \ _ -> Nat -> Bool )
        |    ( natElim
        |        ( \ _ -> Bool )
        |        True
        |        ( \ n' _ -> False ) )
        |    ( \ m' rec_m' -> natElim
        |                       ( \ _ -> Bool )
        |                       False
        |                       ( \ n' _ -> rec_m' n' ))""".stripMargin)
    .interpret("""let Prop = boolElim (\ _ -> *) Void Unit""")
    .interpret(
      """let pNatEqRefl =
        |  natElim
        |    (\ n -> Prop (natEq n n))
        |    U
        |    (\ n' rec -> rec)
        |  :: forall (n :: Nat) . Prop (natEq n n)""".stripMargin)
    .interpret("""let Not = (\ a -> a -> Void) :: * -> *""")
//    .interpret("""""")
//    .interpret("""""")
//    .interpret("""""")
//    .interpret("""""")
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
