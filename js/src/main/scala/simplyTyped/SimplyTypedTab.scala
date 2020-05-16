package simplyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object SimplyTypedTab {

  case class State(input: String = "",
                           interpreterState: InterpreterState = InterpreterState.initial,
                           history: Seq[HistoryEntry] = Seq.empty)

  val simplyTypedTab =
    ScalaComponent.builder[Unit]("SimplyTypedTab")
      .initialState(State())
      .renderBackend[Backend]
      .build

  class Backend($: BackendScope[Unit, State]) {

    val dataToggle = VdomAttr("data-toggle")
    val dataTarget = VdomAttr("data-target")

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
            "λ> ", <.code(input),
            InterpreterOutcomeView.interpreterOutcomeView(result)
          )
        }
      <.div(^.`class` := "tab-pane fade show active", ^.id := "simply",
        <.div(^.`class` := "btn-group", ^.role := "group",
          <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", dataToggle := "collapse", dataTarget := "#instructions-table", "Show/hide instructions"),
          <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", dataToggle := "collapse", dataTarget := "#bindings-table", "Show/hide bindings"),
          <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", "Add Church Numerals", ^.onClick --> onAddChurchNumeralsPressed),
          <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", "Reset", ^.onClick --> onReset),
        ),
        <.p(),
        <.div(^.`class` := "collapse show", ^.id := "instructions-table",
          InstructionsTable.instructionsTable(),
          <.p()),
        <.p(),
        <.div(^.`class` := "collapse", ^.id := "bindings-table",
          BindingsTable.bindingsTable(interpreterState),
          <.p()),
        historicalResults.toTagMod,
        <.form(
          <.div(^.`class` := "input-group mb-3",
            <.div(^.`class` := "input-group-prepend",
              <.span(^.`class` := "input-group-text", "λ> ")),
            <.input(^.`type` := "text", ^.`class` := "form-control", ^.autoFocus := true, ^.placeholder := "Enter λ-calculus terms here...", ^.onChange ==> onChange, ^.value := input),
            <.div(^.`class` := "input-group-append",
              <.button(^.`class` := "btn btn-primary", ^.`type` := "submit", "Execute", ^.onClick --> onEvaluatePressed)))),
        InterpreterOutcomeView.interpreterOutcomeView(result).unless(input.isEmpty))
    }

    private def onReset: Callback =
      $.setState(State())

    private def onAddChurchNumeralsPressed: Callback =
      $.modState { oldState =>
        val State(_, interpreterState, _) = oldState
        val newInterpreterState = interpreterState merge Examples.churchNumeralsInterpreterState
        oldState.copy(interpreterState = newInterpreterState)
      }

    private def onEvaluatePressed: Callback =
      $.modState { oldState =>
        val State(input, interpreterState, history) = oldState
        val (newInterpreterState, result) = Interpreter.interpret(input)(interpreterState)
        oldState.copy(interpreterState = newInterpreterState, input = "", history = history :+ HistoryEntry(input, result))
      }
  }

}
