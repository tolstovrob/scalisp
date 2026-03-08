package evaluator

import ast.*
import env.Env
import utils.Printer.{valueToString, valueToStringList}

object Evaluator:
  def exprToValue(expr: Expr): Value = expr match
    case Expr.Num(v) => Value.Num(v)
    case Expr.Sym(s) => Value.Sym(s)
    case Expr.Lst(els) => Value.Lst(els.map(exprToValue))

  def eval(expr: Expr, env: Env): Value = expr match
    // primitives
    case Expr.Num(v) => Value.Num(v)
    case Expr.Sym(name) => env.lookup(name).binds(name)

    // special forms
    case Expr.Lst(Expr.Sym("quote") :: rest) => rest match
      case head :: Nil => exprToValue(head)
      case _ => sys.error("quote takes exactly 1 argument")
    
    case Expr.Lst(Expr.Sym("if") :: test :: conseq :: alt :: Nil) =>
      eval(test, env) match
        case Value.Num(0) => eval(alt, env)
        case _ => eval(conseq, env)
    
    case Expr.Lst(Expr.Sym("define") :: Expr.Sym(name) :: toSetExpr :: Nil) =>
      val value = eval(toSetExpr, env)
      env.binds(name) = value
      value

    case Expr.Lst(Expr.Sym("set!") :: Expr.Sym(name) :: toSetExpr :: Nil) =>
      val value = eval(toSetExpr, env)
      env.lookup(name).binds(name) = value
      value

    case Expr.Lst(Expr.Sym("lambda") :: Expr.Lst(params) :: body :: Nil) =>
      val paramNames = params.map {
        case Expr.Sym(name) => name
        case _ => sys.error("Invalid lambda parameters")
      }
      Value.Lambda(env, paramNames, body)

    case Expr.Lst(Expr.Sym("begin") :: exps) =>
      exps.map(eval(_, env)).last

    // general calling
    case Expr.Lst(proc :: args) =>
      eval(proc, env) match
        case Value.Builtin(f) => f(args.map(eval(_, env)))
        case Value.Lambda(closureEnv, params, body) =>
          val argValues = args.map(eval(_, env))
          eval(body, Env(params.zip(argValues).toMap, Some(closureEnv))) 
        case other => sys.error(s"Cannot call non-functions: ${valueToString(other)}")
    
    case _ => sys.error("Invalid expression")