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
      

@main
def repl(): Unit =
  println("Scalist REPL")
  while true do
    print("> ")

    val line = readLine()

    line match
      case "exit" => return
      case _ =>
        try
          val expr = parse(line)
          println(expr)
        catch case e: Exception => println(s"Error: ${e.getMessage}")
    
  
   