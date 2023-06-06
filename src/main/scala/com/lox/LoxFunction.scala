package com.lox

trait LoxFunction(declaration: Stmt.Function, closure: Environment)
    extends LoxCallable

def LoxFunction(declaration: Stmt.Function, closure: Environment): LoxFunction =
  new LoxFunction(
    declaration: Stmt.Function,
    closure: Environment,
  ) {
    override def arity: Int = declaration.params.length
    override def call(i: Interpreter, a: List[Any]): Any =
      val env = Environment(closure)
      val prev = i.env
      (declaration.params zip a).foreach((param, arg) => env.define(param, arg))
      i.env = env
      try
        i.interpret(declaration.body)
        null
      catch case Return(value) => value
      finally i.env = prev
    override def toString(): String = "<fn " + declaration.name.lexeme + ">"
  }
