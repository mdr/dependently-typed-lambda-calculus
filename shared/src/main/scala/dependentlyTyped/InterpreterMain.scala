package dependentlyTyped

import scala.io.StdIn.readLine

object InterpreterMain extends App {

  var interpreterState = InterpreterState.initial

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

  // let add = natElim (λ_ -> Nat -> Nat) (λn -> n) (λ_ rec n -> Succ (rec n))

}
