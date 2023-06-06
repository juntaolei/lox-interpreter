package com.lox

trait LoxCallable {
  def arity: Int
  def call(i: Interpreter, a: List[Any]): Any
}
