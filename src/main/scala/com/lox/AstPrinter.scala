package com.lox

import Expr.*
import java.lang.StringBuilder

trait AstPrinter {
  def print(e: Expr): String
}

def AstPrinter: AstPrinter = new AstPrinter {
  override def print(e: Expr): String =
    e match
      case Binary(left, operator, right) =>
        parenthesize(operator.lexeme, left, right)
      case Grouping(expression) =>
        parenthesize("group", expression)
      case Literal(value) =>
        if value == null then "nil"
        else value.toString
      case Unary(operator, right) =>
        parenthesize(operator.lexeme, right)

  def parenthesize(name: String, exprs: Expr*): String =
    val builder = new StringBuilder()
    builder
      .append("(")
      .append(name)
    exprs.foreach(expr =>
      builder
        .append(" ")
        .append(print(expr))
    )
    builder.append(")")
    builder.toString
}
