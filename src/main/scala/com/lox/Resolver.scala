package com.lox

import Expr.*
import Stmt.*
import java.util.Stack
import java.{util => ju}
import java.util.HashMap
import scala.annotation.tailrec

trait Resolver(interpreter: Interpreter) {
  def resolve(stmts: List[Stmt]): Unit
}

def Resolver(interpreter: Interpreter): Resolver = new Resolver(interpreter) {
  enum FunctionType:
    case NONE
    case FUNCTION

  val scopes: Stack[ju.Map[String, Boolean]] = new Stack()

  var currentFunction = FunctionType.NONE

  override def resolve(stmts: List[Stmt]): Unit =
    stmts.foreach(stmt => resolve(stmt))

  def beginScope =
    scopes.push(new HashMap[String, Boolean]())

  def endScope =
    scopes.pop

  def declare(name: Token) =
    if !scopes.isEmpty then
      val scope = scopes.peek
      if scope.containsKey(name.lexeme) then
        Lox.error(name, "Already a variable with this name in this scope.");
      scope.put(name.lexeme, false)

  def define(name: Token) =
    if !scopes.isEmpty then scopes.peek.put(name.lexeme, true)

  def resolve(stmt: Stmt): Unit =
    stmt match
      case Block(statements) =>
        beginScope
        resolve(statements)
        endScope
      case Expression(expression) =>
        resolve(expression)
      case Function(name, params, body) =>
        declare(name)
        define(name)
        resolveFunction(Function(name, params, body), FunctionType.FUNCTION)
      case If(condition, thenBranch, elseBranch) =>
        resolve(condition)
        resolve(thenBranch)
        if elseBranch != null then resolve(elseBranch)
      case Print(expression) =>
        resolve(expression)
      case Return(keyword, value) =>
        if currentFunction == FunctionType.NONE then
          Lox.error(keyword, "Can't return from top-level code.")
        if value != null then resolve(value)
      case Var(name, initializer) =>
        declare(name)
        if initializer != null then resolve(initializer)
        define(name)
      case While(condition, body) =>
        println((condition, " <> ", body))
        resolve(condition)
        resolve(body)

  def resolve(expr: Expr): Unit =
    expr match
      case Assign(name, value) =>
        resolve(value)
        resolveLocal(expr, name)
      case Binary(left, _, right) =>
        resolve(left)
        resolve(right)
      case Call(callee, paren, arguments) =>
        resolve(callee)
        arguments.foreach(argument => resolve(argument))
      case Grouping(expression) =>
        resolve(expression)
      case Literal(_) => ()
      case Logical(left, _, right) =>
        resolve(left)
        resolve(right)
      case Unary(_, right) =>
        resolve(right)
      case Variable(name) =>
        if !scopes.isEmpty && scopes.peek.containsKey(
            name.lexeme
          ) && scopes.peek.get(name.lexeme) == false
        then
          Lox.error(name, "Can't read local variable in its own initializer.")
        resolveLocal(expr, name)

  def resolveLocal(expr: Expr, name: Token) =
    @tailrec
    def aux(i: Int): Unit =
      if i >= 0 then
        if scopes.get(i).containsKey(name.lexeme) then
          println((i, scopes, scopes.size - 1 - i))
          interpreter.resolve(expr, scopes.size - 1 - i)
        else aux(i - 1)
    aux(scopes.size - 1)

  def resolveFunction(function: Function, functionType: FunctionType) =
    val enclosingFunction = currentFunction
    currentFunction = functionType
    beginScope
    function.params.foreach(param =>
      declare(param)
      define(param)
    )
    resolve(function.body)
    endScope
    currentFunction = enclosingFunction
}
