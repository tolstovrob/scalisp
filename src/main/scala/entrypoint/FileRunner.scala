package entrypoint

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
    val source = Source.fromFile(filename, "UTF-8")
    var program = source.mkString
    source.close()
    
    if program.nonEmpty && program.charAt(0) == 0xFEFF then
      program = program.substring(1)
    
    program = program.replace("\r\n", "\n").replace("\r", "\n")
    program = program.trim
    
    if program.isEmpty then
      println("Error: File is empty")
      return

    program = s"(begin\n$program\n)"
    
    val expr = Parser.parse(program)
    val value = eval(expr, globalEnv)
    println(valueToString(value))
    
  catch
    case e: java.io.FileNotFoundException =>
      println(s"Error: File not found: $filename")
    case e: Exception =>
      println(s"Error: ${e.getMessage}")