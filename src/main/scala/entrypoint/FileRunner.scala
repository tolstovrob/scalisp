package runner

import scala.io.Source
import parser.Parser
import evaluator.Evaluator.*
import builtins.Builtins
import env.Env
import utils.Printer.valueToString

@main
def runFile(filename: String): Unit =
  val env = Builtins.builtins
  val globalEnv = Env(env, None)
  
  try
    val source = Source.fromFile(filename)
    val program = source.mkString
    source.close()
    
    println(s"[SCALISP]: running $filename")
    val expr = Parser.parse(program)
    val value = eval(expr, globalEnv)
    println(valueToString(value))
  catch
    case e: java.io.FileNotFoundException =>
      println(s"Error: File not found: $filename")
    case e: Exception =>
      println(s"Error: ${e.getMessage}")