package builtins

import ast.Value
import utils.Printer.{valueToString, valueToStringList}

object ArithmeticOps:
  val builtins: Map[String, Value] = Map(
    "+" -> Value.Builtin { args =>
      val numbers = args.map {
        case Value.Num(d) => d
        case other => sys.error(s"+ expects numbers, got: ${valueToString(other)}")
      }
      Value.Num(numbers.sum)
    },
    
    "-" -> Value.Builtin {
      case List(Value.Num(a)) => Value.Num(-a)
      case List(Value.Num(a), Value.Num(b)) => Value.Num(a - b)
      case other => sys.error(s"- expects 1 or 2 numbers, got: ${valueToStringList(other)}")
    },
    
    "*" -> Value.Builtin { args =>
      val numbers = args.map {
        case Value.Num(d) => d
        case other => sys.error(s"* expects numbers, got: ${valueToString(other)}")
      }
      Value.Num(numbers.product)
    },
    
    "/" -> Value.Builtin {
      case List(Value.Num(a), Value.Num(b)) => Value.Num(a / b)
      case other => sys.error(s"/ expects 2 numbers, got: ${valueToStringList(other)}")
    }
  )