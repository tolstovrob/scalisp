package builtins

import ast.Value
import env.Env
import utils.Printer.{valueToString, valueToStringList}

object Builtins:
  def builtins: Map[String, Value] =
    ArithmeticOps.builtins ++
    ListOps.builtins ++
    ComparisonOps.builtins