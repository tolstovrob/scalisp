package builtins

import ast.Value
import utils.Printer.{valueToString, valueToStringList}

object ListOps:
  val builtins: Map[String, Value] = Map(
    "list" -> Value.Builtin(args => Value.Lst(args)),
    
    "car" -> Value.Builtin {
      case List(Value.Lst(x :: _)) => x
      case List(Value.Lst(Nil)) => sys.error("car expects non-empty list, got: ()")
      case List(other) => sys.error(s"car expects list, got: ${valueToString(other)}")
      case other => sys.error(s"car expects 1 argument, got: ${valueToStringList(other)}")
    },
    
    "cdr" -> Value.Builtin {
      case List(Value.Lst(_ :: xs)) => Value.Lst(xs)
      case List(Value.Lst(Nil)) => sys.error("cdr expects non-empty list, got: ()")
      case List(other) => sys.error(s"cdr expects list, got: ${valueToString(other)}")
      case other => sys.error(s"cdr expects 1 argument, got: ${valueToStringList(other)}")
    },
    
    "cons" -> Value.Builtin {
      case List(x, Value.Lst(xs)) => Value.Lst(x :: xs)
      case List(_, Value.Lst(Nil)) => Value.Lst(List())
      case List(x, other) => sys.error(s"cons expects list as second argument, got: ${valueToString(other)}")
      case other => sys.error(s"cons expects 2 arguments, got: ${valueToStringList(other)}")
    },
    
    "null?" -> Value.Builtin {
      case List(Value.Lst(Nil)) => Value.Num(1)
      case List(Value.Lst(_)) => Value.Num(0)
      case List(other) => sys.error(s"null? expects list, got: ${valueToString(other)}")
      case other => sys.error(s"null? expects 1 argument, got: ${valueToStringList(other)}")
    }
  )