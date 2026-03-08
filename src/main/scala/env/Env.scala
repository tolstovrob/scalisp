package env

import scala.collection.mutable
import ast.Value

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