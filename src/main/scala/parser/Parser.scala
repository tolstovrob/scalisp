package parser

import scala.annotation.tailrec
import scala.util.Try
import ast.Expr

object Parser:
  def parse(program: String): Expr =
    val normalized = program
      .replace("\n", " ")
      .replace("\r", " ")
      .replace("\t", " ")

    val tokens = normalized
      .replace("(", " ( ")
      .replace(")", " ) ")
      .replace("'", " ' ")
      .trim
      .split("\\s+")
      .filter(_.nonEmpty)
      .toList
      
    def readFrom(tokens: List[String]): (Expr, List[String]) = tokens match
      case Nil => sys.error("Unexpected EOF")
      case "'" :: rest =>
        val (expr, next) = readFrom(rest)
        (Expr.Lst(List(Expr.Sym("quote"), expr)), next)
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