scalaVersion := "3.8.2"

lazy val root = (project in file("."))
  .settings(
    Compile / mainClass := Some("repl.repl"),
  )