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
      case Right(InterpreterResult.Assume(name, HasKind(kind))) => println(s"$name :: $kind")
      case Right(InterpreterResult.Assume(name, HasType(typ))) => println(s"$name :: $typ")
      case Right(InterpreterResult.Evaluated(name, value, typ)) =>
        println(s"$name :: $typ")
        println(value)
    }
  }

}
