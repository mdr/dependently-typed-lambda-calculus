import dependentlyTyped.DependentlyTypedTab
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import simplyTyped.{HistoryEntry, InterpreterState, SimplyTypedTab}

object App {
  val dataToggle = VdomAttr("data-toggle")

  private case class State(input: String = "",
                           interpreterState: InterpreterState = InterpreterState.initial,
                           history: Seq[HistoryEntry] = Seq.empty)

  val app = ScalaComponent.static("No args")(
    <.div(
      <.nav(^.className := "navbar navbar-expand-lg navbar-dark bg-dark")(
        <.div(^.className := "container-fluid")(
          <.a(^.className := "navbar-brand", ^.href := "#")("λ-Calculus"),
          <.div(^.className := "collapse navbar-collapse", ^.id := "navbarsExample07")(
            <.ul(^.className := "navbar-nav mr-auto")(
              <.li(^.className := "nav-item active")(
                <.a(^.className := "nav-link", ^.href := "#")(
                  "Dependently-typed",
                  <.span(^.className := "sr-only")("(current)")
                )
              ),
              <.li(^.className := "nav-item")(
                <.a(^.className := "nav-link", ^.href := "#")("Simply-typed")
              ),
            ),
          )
        )
      ),
      <.div(^.`class` := "container-fluid",
        <.p(),
        <.p("A Scala port of ", <.a(^.href := "https://www.andres-loeh.de/LambdaPi/", <.em("A Tutorial Implementation of a Dependently Typed Lambda Calculus")), " by Andres Löh, Conor McBride and Wouter Swierstra. Code can be found on ", <.a(^.href := "https://github.com/mdr/dependently-typed-lambda-calculus", "Github"), "."),
        <.ul(^.`class` := "nav nav-tabs", ^.id := "appTab",
          <.li(^.`class` := "nav-item",
            <.a(^.`class` := "nav-link active", ^.id := "dependently-tab", ^.href := "#dependently", dataToggle := "tab", "Dependently-typed")),
          <.li(^.`class` := "nav-item",
            <.a(^.`class` := "nav-link", ^.id := "simply-tab", ^.href := "#simply", dataToggle := "tab", "Simply-typed"))),
        <.div(^.`class` := "tab-content", ^.id := "appTabContent",
          <.div(^.`class` := "tab-pane fade", ^.id := "simply", SimplyTypedTab.simplyTypedTab()),
          <.div(^.`class` := "tab-pane fade show active", ^.id := "dependently", DependentlyTypedTab.dependentlyTypedTab())
        ))))
}
