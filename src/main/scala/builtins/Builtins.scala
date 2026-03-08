package builtins

import ast.Value
import env.Env
import utils.Printer.{valueToString, valueToStringList}

trait BuiltinProvider:
  def builtins: Map[String, Value]

object Builtins extends BuiltinProvider:
  def builtins: Map[String, Value] =
    ArithmeticOps.builtins ++
    ListOps.builtins ++
    ComparisonOps.builtins