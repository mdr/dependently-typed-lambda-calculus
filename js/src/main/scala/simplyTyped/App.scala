package simplyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object App {

  case class State(input: String = "",
                   interpreterState: InterpreterState = Examples.churchNumeralsInterpreterState)

  val app =
    ScalaComponent.builder[Unit]("App")
      .initialState(State())
      .renderBackend[Backend]
      .build

  class Backend($: BackendScope[Unit, State]) {

    def onChange(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(input = newValue))
    }

    def render(state: State) = {
      val State(input, interpreterState) = state
      val (_, result) = Interpreter.interpret(input)(interpreterState)
      <.div(^.`class` := "container",
        <.h2("Simply-Typed Lambda Calculus"),
        <.h3("Bindings"),
        BindingsTable.bindingsTable(interpreterState),
        <.div(
          <.input(^.`class` := "form-control", ^.onChange ==> onChange, ^.value := input)),
        result match {
          case Left(error) =>
            <.div(^.`class` := "alert alert-danger", ^.role := "alert", s"Error: ${error}")
          case Right(InterpreterResult.Evaluated(_, value, typ)) => <.div(s"Value: ${value}, type: $typ")
        }
      )
    }
  }

}
