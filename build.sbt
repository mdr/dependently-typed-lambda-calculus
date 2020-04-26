import sbtcrossproject.CrossPlugin.autoImport.crossProject

ThisBuild / scalaVersion := "2.13.1"

lazy val root = project.in(file(".")).
  aggregate(foo.js, foo.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )

lazy val foo = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "dependently-typed-lambda-calculus",
    version := "0.1",
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.1" % "test",
  ).
  jvmSettings().
  jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin)).
  jsSettings(
    npmDependencies in Compile ++= Seq(
      "react" -> "16.7.0",
      "react-dom" -> "16.7.0"),
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("simplyTyped.Main"),
    libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "1.6.0"
  )
