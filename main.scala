import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try
import scala.io.StdIn.readLine

enum Expr:
  case Sym(name: String)
  case Num(value: Double)
  case Lst(elements: List[Expr])

enum Value:
  case Num(v: Double)
  case Str(s: String)
  case Sym(name: String)
  case Lst(elements: List[Value])
  case Builtin(f: List[Value] => Value)
  case Lambda(env: Env, params: List[String], body: Expr)


case class Env(binds: mutable.Map[String, Value], outer: Option[Env]):
  def lookup(name: String): Env =
    if binds.contains(name)
    then this
    else outer match
      case None => sys.error(s"Undefined name: $name")
      case Some(env) => env.lookup(name)

object Env:
  def apply(binds: Map[String, Value], outer: Option[Env]): Env =
    new Env(mutable.Map.from(binds), outer)


def parse(program: String): Expr =
  val tokens = program
    .replace("(", " ( ")
    .replace(")", " ) ")
    .trim
    .split("\\s+")
    .filter(_.nonEmpty)
    .toList
    
  def readFrom(tokens: List[String]): (Expr, List[String]) = tokens match
    case Nil => sys.error("Unexpected EOF")
    case "(" :: rest =>
      val (elements, next) = readSeq(rest)
      (Expr.Lst(elements), next)
    case ")" :: _ => sys.error("Unexpected )")
    case token :: rest =>
      val expr = Try(token.toDouble).toOption match
        case None => Expr.Sym(token)
        case Some(value) => Expr.Num(value)
      (expr, rest)
    
  @tailrec
  def readSeq(tokens: List[String], acc: List[Expr] = Nil): (List[Expr], List[String]) = tokens match
    case ")" :: rest => (acc.reverse, rest)
    case _ =>
      val (expr, next) = readFrom(tokens)
      readSeq(next, expr :: acc)

  readFrom(tokens) match
    case (expr, Nil) => expr
    case _ => sys.error("Invalid syntax")    
      

def exprToValue(expr: Expr): Value = expr match
  case Expr.Num(v) => Value.Num(v)
  case Expr.Sym(s) => Value.Sym(s)
  case Expr.Lst(els) => Value.Lst(els.map(exprToValue))


def eval(expr: Expr, env: Env): Value = expr match
  //
  // primitives
  //
  case Expr.Num(v) => Value.Num(v)
  case Expr.Sym(name) => env.lookup(name).binds(name)

  //  
  // special forms
  //
  case Expr.Lst(Expr.Sym("quote") :: arg :: Nil) => exprToValue(arg)
  
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

  //
  // general calling
  //
  case Expr.Lst(proc :: args) =>
    eval(proc, env) match
      case Value.Builtin(f) => f(args.map(eval(_, env)))
      case Value.Lambda(closureEnv, params, body) =>
        val argValues = args.map(eval(_, env))
        eval(body, Env(params.zip(argValues).toMap, Some(closureEnv))) 
  
  case _ => sys.error("Invalid expression")


def globalEnv: Env =
  val builtins = Map[String, Value](
    "+"    -> Value.Builtin(args => Value.Num(args.map { case Value.Num(d) => d }.sum)),
    "-"    -> Value.Builtin {
      case List(Value.Num(a)) => Value.Num(-a)
      case List(Value.Num(a), Value.Num(b)) => Value.Num(a - b)
      case _ => sys.error("- expects 1 or 2 numbers")
    },
    "*"    -> Value.Builtin(args => Value.Num(args.map { case Value.Num(d) => d }.product)),
    "/"    -> Value.Builtin {
      case List(Value.Num(a), Value.Num(b)) => Value.Num(a / b)
      case _ => sys.error("/ expects 2 numbers")
    },
    "="    -> Value.Builtin(args => Value.Num(if args.map { case Value.Num(d) => d }.sliding(2).forall { case List(a,b) => a == b } then 1 else 0)),
    "<"    -> Value.Builtin(args => Value.Num(if args.map { case Value.Num(d) => d }.sliding(2).forall { case List(a,b) => a < b } then 1 else 0)),
    ">"    -> Value.Builtin(args => Value.Num(if args.map { case Value.Num(d) => d }.sliding(2).forall { case List(a,b) => a > b } then 1 else 0)),
    "list" -> Value.Builtin(args => Value.Lst(args)),
    "car"  -> Value.Builtin {
      case List(Value.Lst(x :: _)) => x
      case _ => sys.error("car expects non-empty list")
    },
    "cdr"  -> Value.Builtin {
      case List(Value.Lst(_ :: xs)) => Value.Lst(xs)
      case _ => sys.error("cdr expects non-empty list")
    },
    "cons" -> Value.Builtin {
      case List(x, Value.Lst(xs)) => Value.Lst(x :: xs)
      case _ => sys.error("cons expects element and list")
    },
    "null?" -> Value.Builtin {
      case List(Value.Lst(Nil)) => Value.Num(1)
      case _ => Value.Num(0)
    },
    "eq?"  -> Value.Builtin(args => Value.Num(if args.head == args(1) then 1 else 0))
  )
  Env(builtins, None)


def valueToString(v: Value): String = v match
  case Value.Num(v) => v.toString
  case Value.Str(s) => s
  case Value.Sym(name) => name
  case Value.Lst(els) => els.map(valueToString).mkString("(", " ", ")")
  case Value.Builtin(_) => "<builtin>"
  case Value.Lambda(_, _, _) => "<lambda>"


@main
def repl(): Unit =
  val env = globalEnv
  println("Scalist REPL")

  while true do
    print("> ")
    val line = readLine()

    line match
      case "exit" => return
      case _ =>
        try
          val expr = parse(line)
          val value = eval(expr, env)
          println(valueToString(value))
        catch case e: Exception => println(s"Error: ${e.getMessage}")
    