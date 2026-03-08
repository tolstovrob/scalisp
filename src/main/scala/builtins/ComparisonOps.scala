package builtins

import ast.Value
import utils.Printer.{valueToString, valueToStringList}

object ComparisonOps:
  val builtins: Map[String, Value] = Map(
    "=" -> Value.Builtin { args =>
      val numbers = args.map {
        case Value.Num(d) => d
        case other => sys.error(s"= expects numbers, got: ${valueToString(other)}")
      }
      val result = numbers.sliding(2).forall {
        case List(a, b) => a == b
        case _ => true
      }
      Value.Num(if result then 1 else 0)
    },
    
    "<" -> Value.Builtin { args =>
      val numbers = args.map {
        case Value.Num(d) => d
        case other => sys.error(s"< expects numbers, got: ${valueToString(other)}")
      }
      val result = numbers.sliding(2).forall {
        case List(a, b) => a < b
        case _ => true
      }
      Value.Num(if result then 1 else 0)
    },
    
    ">" -> Value.Builtin { args =>
      val numbers = args.map {
        case Value.Num(d) => d
        case other => sys.error(s"> expects numbers, got: ${valueToString(other)}")
      }
      val result = numbers.sliding(2).forall {
        case List(a, b) => a > b
        case _ => true
      }
      Value.Num(if result then 1 else 0)
    },
    
    "eq?" -> Value.Builtin {
      case List(a, b) => Value.Num(if a == b then 1 else 0)
      case other => sys.error(s"eq? expects 2 arguments, got: ${valueToStringList(other)}")
    }
  )