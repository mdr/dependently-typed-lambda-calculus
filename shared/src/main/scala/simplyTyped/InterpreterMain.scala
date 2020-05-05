package simplyTyped

import scala.io.StdIn.readLine

object InterpreterMain extends App {

  var interpreterState = Examples.churchNumeralsInterpreterState

  while (true) {
    val line = readLine("λ> ")
    val (newState, resultEither) = Interpreter.interpret(line)(interpreterState)
    interpreterState = newState
    resultEither match {
      case Left(error) => println(error)
      case Right(InterpreterResult.Assume(assumptions, _)) =>
        for (InterpreterResult.Assumption(name, info) <- assumptions)
          info match {
            case HasKind(kind) =>
              println(s"$name :: $kind")
            case HasType(typ) =>
              println(s"$name :: $typ")
          }
      case Right(InterpreterResult.Evaluated(name, value, typ, _)) =>
        println(s"$name :: $typ")
        println(value)
    }
  }

}
