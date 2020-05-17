package dependentlyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object DependentlyTypedTab {

  case class State(input: String = "",
                   interpreterState: InterpreterState = InterpreterState.prelude,
                   history: Seq[HistoryEntry] = Seq.empty)

  val dependentlyTypedTab =
    ScalaComponent.builder[Unit]("DependentlyTypedTab")
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
            "λπ> ", <.code(input),
            InterpreterOutcomeView.interpreterOutcomeView(result)
          )
        }
      <.div(^.`class` := "tab-pane fade show active", ^.id := "simply",
        <.div(^.`class` := "btn-group", ^.role := "group",
          <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", dataToggle := "collapse", dataTarget := "#instructions-table", "Show/hide instructions"),
          <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", dataToggle := "collapse", dataTarget := "#bindings-table", "Show/hide bindings"),
          <.button(^.`class` := "btn btn-outline-secondary", ^.`type` := "button", "Reset", ^.onClick --> onReset),
        ),
        <.p(),
        <.div(^.`class` := "collapse", ^.id := "instructions-table",
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
              <.span(^.`class` := "input-group-text", "λπ> ")),
            <.input(^.`type` := "text", ^.`class` := "form-control", ^.autoFocus := true, ^.placeholder := "Enter λπ-calculus terms here...", ^.onChange ==> onChange, ^.value := input),
            <.div(^.`class` := "input-group-append",
              <.button(^.`class` := "btn btn-primary", ^.`type` := "submit", "Execute", ^.onClick --> onEvaluatePressed)))),
        InterpreterOutcomeView.interpreterOutcomeView(result).unless(input.isEmpty))
    }

    private def onReset: Callback = $.setState(State())

    private def onEvaluatePressed: Callback =
      $.modState { oldState =>
        val State(input, interpreterState, history) = oldState
        val (newInterpreterState, result) = Interpreter.interpret(input)(interpreterState)
        oldState.copy(interpreterState = newInterpreterState, input = "", history = history :+ HistoryEntry(input, result))
      }
  }

}
