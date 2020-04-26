package simplyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import simplyTyped.InterpreterResult.InterpreterOutcome

object InterpreterOutcomeView {

  val interpreterOutcomeView = ScalaComponent.builder[InterpreterOutcome]("interpreterOutcomeView")
    .render_P {
      case Left(error) =>
        <.div(^.`class` := "alert alert-danger", ^.role := "alert", s"Error: ${error}")
      case Right(InterpreterResult.Assume(name, HasKind(kind))) =>
        <.div(^.`class` := "alert alert-info", ^.role := "alert", s"$name :: $kind")
      case Right(InterpreterResult.Assume(name, HasType(typ))) =>
        <.div(^.`class` := "alert alert-info", ^.role := "alert", s"$name :: $typ")
      case Right(InterpreterResult.Evaluated(name, value, typ)) =>
        <.div(^.`class` := "alert alert-success", ^.role := "alert",
          s"$name :: $typ",
          <.br(),
          value.toString)
    }
    .build


}

object App {

  case class State(input: String = "",
                   interpreterState: InterpreterState = Examples.churchNumeralsInterpreterState,
                   history: Seq[HistoryEntry] = Seq.empty)

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
      val State(input, interpreterState, history) = state
      val (_, result) = Interpreter.interpret(input)(interpreterState)

      val historicalResults =
        history.map { case HistoryEntry(input, result) =>
          <.div(
            input,
            InterpreterOutcomeView.interpreterOutcomeView(result)
          )
        }
      <.div(^.`class` := "container",
        <.h2("Simply-Typed Lambda Calculus"),
        <.h3("Bindings"),
        BindingsTable.bindingsTable(interpreterState),
        historicalResults.toTagMod,
        <.div(^.`class` := "input-group mb-3",
          <.input(^.`type` := "text", ^.`class` := "form-control", ^.placeholder := "Enter λ-calculus terms here...", ^.onChange ==> onChange, ^.value := input),
          <.div(^.`class` := "input-group-append",
            <.button(^.`class` := "btn btn-outline-primary", ^.`type` := "button", "Evaluate", ^.onClick --> onButtonPressed))),
        InterpreterOutcomeView.interpreterOutcomeView(result).unless(input.isEmpty)
      )

    }

    def onButtonPressed: Callback =
      $.modState { oldState =>
        val State(input, interpreterState, history) = oldState
        val (newInterpreterState, result) = Interpreter.interpret(input)(interpreterState)
        oldState.copy(interpreterState = newInterpreterState, input = "", history = history :+ HistoryEntry(input, result))
      }
  }

}
