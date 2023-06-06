package com.lox

import java.util.HashMap
import com.lox
import scala.annotation.tailrec
import java.{util => ju}

trait Environment(e: Environment) {
  val values: ju.Map[String, Any]
  val enclosing: Environment

  def define(name: String, obj: Any): Unit
  def define(token: Token, obj: Any): Unit
  def assign(token: Token, obj: Any): Unit
  def get(token: Token): Any
  def getAt(distance: Integer, name: String): Any
  def ancestor(distance: Integer): Environment
  def assignAt(distance: Integer, name: Token, value: Any): Unit
}

def Environment(e: Environment = null): Environment = new Environment(
  e: Environment
) {
  val values = new HashMap[String, Any]()
  val enclosing: Environment = e

  override def define(name: String, obj: Any): Unit = values.put(name, obj)

  override def define(token: Token, obj: Any): Unit =
    values.put(token.lexeme, obj)

  override def assign(token: Token, obj: Any): Unit =
    if values.containsKey(token.lexeme) then values.put(token.lexeme, obj)
    else if enclosing != null then enclosing.assign(token, obj)
    else throw RuntimeError(token, "Undefined variable '" + token.lexeme + "'.")

  override def get(token: Token): Any =
    if values.containsKey(token.lexeme) then values.get(token.lexeme)
    else if enclosing != null then enclosing.get(token)
    else throw RuntimeError(token, "Undefined variable '" + token.lexeme + "'.")

  override def getAt(distance: Integer, name: String): Any =
    ancestor(distance).values.get(name)

  override def ancestor(distance: Integer): com.lox.Environment =
    val cenv = this
    @tailrec
    def aux(current: Environment, i: Int): Environment =
      if i < distance then aux(current.enclosing, i + 1)
      else current
    aux(cenv, 0)

  override def assignAt(distance: Integer, name: Token, value: Any): Unit =
    ancestor(distance).values.put(name.lexeme, value)
}
