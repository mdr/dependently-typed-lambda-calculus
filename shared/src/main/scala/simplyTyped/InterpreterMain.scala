package simplyTyped
import scala.io.StdIn.readLine

object InterpreterMain extends App {

  var interpreterState = InterpreterState.initial
  val session =
    for {
      _ <- Interpreter.interpret("assume a :: *")
      _ <- Interpreter.interpret("let zero = (λf x -> x) :: (a -> a) -> (a -> a)")
      _ <- Interpreter.interpret("let one = (λf x -> f x) :: (a -> a) -> (a -> a)")
      _ <- Interpreter.interpret("let succ = (λn f x -> f (n f x)) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
      _ <- Interpreter.interpret("let two = succ one")
      _ <- Interpreter.interpret("let three = succ two")
      _ <- Interpreter.interpret("let four = succ three")
      _ <- Interpreter.interpret("let five = succ four")
      _ <- Interpreter.interpret("let six = succ five")
      _ <- Interpreter.interpret("let add = (λm n f x -> m f (n f x)) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
      _ <- Interpreter.interpret("let mult = (λm n f x -> m (n f) x) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
      twoPlusFour <- Interpreter.eval("add two four")
      threeTimesTwo <- Interpreter.eval("mult three two")
    } yield twoPlusFour -> threeTimesTwo
  interpreterState = session(interpreterState)._1

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
