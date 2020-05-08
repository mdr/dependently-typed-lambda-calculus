package dependentlyTyped

import scala.io.StdIn.readLine

object InterpreterMain extends App {

  var interpreterState = InterpreterState.prelude

  while (true) {
    val line = readLine("λπ> ")
    val (newState, resultEither) = Interpreter.interpret(line)(interpreterState)
    interpreterState = newState
    resultEither match {
      case Left(error) => println(error)
      case Right(InterpreterResult.Assume(assumptions, _)) =>
        for (InterpreterResult.Assumption(name, typ) <- assumptions)
          println(s"$name :: $typ")
      case Right(InterpreterResult.Evaluated(name, value, typ, _)) =>
        println(s"$name :: $typ")
        println(value)
    }
  }

  // let plus = natElim (λ_ -> Nat -> Nat) (λn -> n) (λ_ rec n -> Succ (rec n))
  // let pred = natElim (\_ -> Nat) 0 (\n' _rec -> n')
  // let replicate = (natElim (\n -> forall (a :: *) . a -> Vec a n) (\a _ -> Nil a) (\n' rec_n' a x -> Cons a n' x (rec_n' a x))) :: forall (n :: Nat) . forall (a :: *) . a -> Vec a n

  // let append = (\a -> vecElim a (\m _ -> forall (n :: Nat) . Vec a n -> Vec a (plus m n)) (\_ v -> v) (\m v vs rec n w -> Cons a (plus m n) v (rec n w)))  ::  forall (a :: *) (m :: Nat) (v :: Vec a m) (n :: Nat) (w :: Vec a n). Vec a (plus m n)

  // let nat1Elim = ( \ m m0 m1 ms -> natElim m m0 (\ p rec -> natElim (\ n -> m (Succ n)) m1 ms p) ) :: forall (m :: Nat -> *) . m 0 -> m 1 -> (forall n :: Nat . m (Succ n) -> m (Succ (Succ n))) -> forall (n :: Nat) . m n
}
