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
  println("Type :exit to quit")
  
  def processLine(line: String): Unit =
    try
      val expr = Parser.parse(line)
      val value = eval(expr, globalEnv)
      println(valueToString(value))
    catch case e: Exception => println(s"Error: ${e.getMessage}")

  def readMultiline(acc: String, openParens: Int): String =
    if openParens == 0 && acc.nonEmpty then
      acc
    else
      val prompt = if openParens > 0 then "... " else "> "
      print(prompt)
      val line = readLine()
      
      if line == null || line == ":exit" then
        sys.exit(0)
      else
        val newAcc = if acc.isEmpty then line else s"$acc\n$line"
        val newOpen = line.count(_ == '(') - line.count(_ == ')')
        readMultiline(newAcc, openParens + newOpen)

  while true do
    try
      val input = readMultiline("", 0)
      processLine(input)
    catch
      case e: Exception => println(s"Error: ${e.getMessage}")