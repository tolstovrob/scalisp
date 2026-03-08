package utils

import ast.Value

object Printer:
  def valueToString(v: Value): String = v match
    case Value.Num(v) => v.toString
    case Value.Str(s) => s
    case Value.Sym(name) => name
    case Value.Lst(els) => els.map(valueToString).mkString("(", " ", ")")
    case Value.Builtin(_) => "<builtin>"
    case Value.Lambda(_, _, _) => "<lambda>"

  def valueToStringList(values: List[Value]): String =
    values.map(valueToString).mkString("(", " ", ")")