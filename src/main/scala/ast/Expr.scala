package ast

enum Expr:
  case Sym(name: String)
  case Num(value: Double)
  case Lst(elements: List[Expr])