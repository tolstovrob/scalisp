package ast

import env.Env

enum Value:
  case Num(v: Double)
  case Str(s: String)
  case Sym(name: String)
  case Lst(elements: List[Value])
  case Builtin(f: List[Value] => Value)
  case Lambda(env: Env, params: List[String], body: Expr)