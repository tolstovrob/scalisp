package repl

import scala.io.StdIn.readLine
import parser.Parser
import evaluator.Evaluator.*
import builtins.Builtins
import env.Env
import utils.Printer.valueToString

@main
def repl(): Unit =
  val env = Builtins.builtins
  val globalEnv = Env(env, None)
  println("[SCALISP]: running REPL")

  while true do
    print("> ")
    val line = readLine()

    line match
      case "exit" => return
      case _ =>
        try
          val expr = Parser.parse(line)
          val value = eval(expr, globalEnv)
          println(valueToString(value))
        catch case e: Exception => println(s"Error: ${e.getMessage}")