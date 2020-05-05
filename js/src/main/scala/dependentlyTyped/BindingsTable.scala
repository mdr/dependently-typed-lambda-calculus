package dependentlyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object BindingsTable {

  val bindingsTable =
    ScalaComponent.builder[InterpreterState]("BindingsTable")
      .render_P(interpreterState => {
        val rows = interpreterState.Γ.infoByName.toSeq
          .sortBy { case (name, y) => interpreterState.letBindings.getOrElse(name.toString, "").toString.length + y.toString.length }
          .collect { case (Name.Global(name), typ) =>
            <.tr(
              <.td(<.code(name)),
              <.td(<.code(interpreterState.letBindings.getOrElse(name, "").toString)),
              <.td(<.code(typ.toString))
            )
          }
        <.table(^.`class` := "table table-bordered",
          <.thead(
            <.tr(
              <.th(^.scope := "col", "Name"),
              <.th(^.scope := "col", "Value"),
              <.th(^.scope := "col", "Type")
            )
          ),
          <.tbody(rows.toTagMod)
        )
      })
      .build

}