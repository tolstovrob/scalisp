repl:
  sbt run

run FILE:
  sbt "runMain runner.runFile {{FILE}}"