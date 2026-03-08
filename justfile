repl:
  sbt run

run FILE:
  sbt "runMain entrypoint.runFile {{FILE}}"