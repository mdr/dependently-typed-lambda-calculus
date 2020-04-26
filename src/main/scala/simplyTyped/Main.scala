//package simplyTyped
//
//import org.scalajs.dom.document
//import japgolly.scalajs.react._
//import japgolly.scalajs.react.vdom.html_<^._
//
//// (λm n f x -> m (n f) x) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))
//case class State(input: String = "",
//                 interpreterState: InterpreterState = {
//                   val zero = Parser.parseTerm("(λf x -> x) :: (a -> a) -> (a -> a)")
//                   val one = Parser.parseTerm("(λf x -> f x) :: (a -> a) -> (a -> a)")
//                   val succ = Parser.parseTerm("(λn f x -> f (n f x)) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
//                   val add = Parser.parseTerm("(λm n f x -> m f (n f x)) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
//                   val mult = Parser.parseTerm("(λm n f x -> m (n f) x) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
//                   InterpreterState(
//                     letBindings = {
//                       Map(
//                         "zero" -> Evaluator.eval(zero),
//                         "one" -> Evaluator.eval(one),
//                         "succ" -> Evaluator.eval(succ),
//                         "add" -> Evaluator.eval(add),
//                         "mult" -> Evaluator.eval(mult)
//                       )
//                     },
//                     assumptions = Map(
//                       "a" -> HasKind(*),
//                       "zero" -> HasType(Parser.parseType("(a -> a) -> (a -> a)")),
//                       "one" -> HasType(Parser.parseType("(a -> a) -> (a -> a)")),
//                       "succ" -> HasType(Parser.parseType("((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")),
//                       "add" -> HasType(Parser.parseType("((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")),
//                       "mult" -> HasType(Parser.parseType("((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))"))
//                     ))
//                 })
//
//class Backend($: BackendScope[Unit, State]) {
//
//  def onChange(e: ReactEventFromInput) = {
//    val newValue = e.target.value
//    $.modState(_.copy(input = newValue))
//  }
//
//  def render(state: State) = {
//    val State(input, interpreterState) = state
//    val result = Interpreter.interpret(input, interpreterState)
//    val rows = interpreterState.Γ.infoByName.toSeq.sortBy(_._1.toString).collect { case (Name.Global(name), info) =>
//      <.tr(
//        <.td(name),
//        <.td(interpreterState.letBindings.getOrElse(name, "").toString),
//        <.td(info match {
//          case HasKind(*) => "*"
//          case HasType(typ) => typ.toString
//        })
//      )
//    }
//    <.div(^.`class` := "container",
//      <.h2("Simply-Typed Lambda Calculus"),
//      <.h3("Bindings"),
//      <.table(^.`class` := "table",
//        <.thead(
//          <.tr(
//            <.th(^.scope := "col", "Name"),
//            <.th(^.scope := "col", "Value"),
//            <.th(^.scope := "col", "Type")
//          )
//        ),
//        <.tbody(rows.toTagMod)
//      ),
//      <.div(
//        <.input(^.`class` := "form-control", ^.onChange ==> onChange, ^.value := input)),
//      result match {
//        case Left(error) =>
//          <.div(^.`class` := "alert alert-danger", ^.role := "alert", s"Error: ${error}")
//        case Right((value, typ)) => <.div(s"Value: ${value}, type: $typ")
//      }
//    )
//  }
//}
//
//object Main {
//  val App =
//    ScalaComponent.builder[Unit]("App")
//      .initialState(State())
//      .renderBackend[Backend]
//      .build
//
//  def main(args: Array[String]): Unit = {
//    val target = document.getElementById("root")
//    App().renderIntoDOM(target)
//  }
//
//}
//
