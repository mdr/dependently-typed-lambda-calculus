import dependentlyTyped.DependentlyTypedTab
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import simplyTyped.SimplyTypedTab

object App {
  val dataToggle = VdomAttr("data-toggle")

  case class State(tab: Int)

  class Backend($: BackendScope[Unit, State]) {
    private def onSimplePressed: Callback = $.modState(_.copy(tab = 1))

    private def onDependentPressed: Callback = $.modState(_.copy(tab = 0))

    def render(state: State) = {
      <.div(
        <.nav(^.className := "navbar navbar-expand-lg navbar-dark bg-dark")(
          <.div(^.className := "container-fluid")(
            <.a(^.className := "navbar-brand", ^.href := "#")("λ-Calculus"),
            <.div(^.className := "collapse navbar-collapse", ^.id := "navbarsExample07")(
              <.ul(^.className := "navbar-nav mr-auto")(
                <.li(^.className := (if (state.tab == 0) "nav-item active" else "nav-item"))(
                  <.a(^.className := "nav-link", ^.href := "#", ^.onClick --> onDependentPressed)(
                    "Dependently-typed"
                  )
                ),
                <.li(^.className := (if (state.tab == 1) "nav-item active" else "nav-item"))(
                  <.a(^.className := "nav-link", ^.href := "#", ^.onClick --> onSimplePressed)(
                    "Simply-typed")
                ),
              ),
            )
          )
        ),
        <.div(^.`class` := "container-fluid",
          <.p(),
          <.p("A Scala port of ", <.a(^.href := "https://www.andres-loeh.de/LambdaPi/", <.em("A Tutorial Implementation of a Dependently Typed Lambda Calculus")), " by Andres Löh, Conor McBride and Wouter Swierstra. Code can be found on ", <.a(^.href := "https://github.com/mdr/dependently-typed-lambda-calculus", "Github"), "."),
          DependentlyTypedTab.dependentlyTypedTab().when(state.tab == 0),
          SimplyTypedTab.simplyTypedTab().when(state.tab == 1)))
    }

  }

  val app = ScalaComponent.builder[Unit]("App")
    .initialState(State(0))
    .renderBackend[Backend]
    .build

}
