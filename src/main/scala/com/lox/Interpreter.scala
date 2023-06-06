package com.lox

import Expr.*
import TokenType.*
import Stmt.*
import scala.annotation.tailrec
import com.lox
import java.util.HashMap

case class RuntimeError(t: Token, s: String) extends RuntimeException
case class Return(value: Any) extends RuntimeException

trait Interpreter {
  val globals: Environment
  var env: Environment

  def interpret(stmts: List[Stmt]): Unit
  def resolve(expr: Expr, depth: Int): Unit
}

def Interpreter: Interpreter = new Interpreter {
  val globals: Environment =
    val gEnv = Environment()
    gEnv.define(
      "clock",
      new LoxCallable {
        override def arity = 0
        override def call(i: lox.Interpreter, a: List[Any]): Any =
          System.currentTimeMillis / 1000
        override def toString(): String = "<native fn>"
      },
    )
    gEnv
  val locals = new HashMap[Expr, Integer]()

  var env: Environment = globals

  override def interpret(stmts: List[Stmt]): Unit =
    try stmts.foreach(stmt => execute(stmt))
    catch
      case RuntimeError(t, s) =>
        Lox.runtimeError(RuntimeError(t, s))

  override def resolve(expr: Expr, depth: Int): Unit =
    locals.put(expr, depth)

  def stringify(obj: Any) =
    if obj == null then "nil"
    else if obj.isInstanceOf[Double] then
      val text = obj.toString;
      if text.endsWith(".0") then text.substring(0, text.length() - 2)
      else text
    else obj.toString

  def isTruthy(obj: Any) =
    if obj == null then false
    else if obj.isInstanceOf[Boolean] then obj.asInstanceOf[Boolean]
    else true

  def isEqual(l: Any, r: Any) =
    if l == null && r == null then true
    else if l == null then false
    else l.equals(r)

  def checkNumOperand(operator: Token, operand: Any) =
    if !operand.isInstanceOf[Double] then
      throw RuntimeError(operator, "Operand must be a number.")

  def checkNumOperands(operator: Token, l: Any, r: Any) =
    if !l.isInstanceOf[Double] || !r.isInstanceOf[Double] then
      throw RuntimeError(operator, "Operands must be numbers.")

  def lookUpVariable(name: Token, expr: Expr) =
    val distance = locals.get(expr)
    if distance != null then env.getAt(distance, name.lexeme)
    else globals.get(name)

  def execute(stmt: Stmt): Unit =
    stmt match
      case Block(statements) =>
        val blockEnv = Environment(env)
        val previous = env
        try
          env = blockEnv
          statements.foreach(stmt => execute(stmt))
        finally env = previous
      case Expression(expression) =>
        evaluate(expression)
      case Function(name, params, body) =>
        val function = LoxFunction(Function(name, params, body), env)
        env.define(name, function)
      case If(condition, thenBranch, elseBranch) =>
        if (isTruthy(evaluate(condition))) then execute(thenBranch)
        else if elseBranch != null then execute(elseBranch)
      case Print(expression) =>
        println(stringify(evaluate(expression)))
      case Stmt.Return(keyword, value) =>
        val valueObj = if value != null then evaluate(value) else null
        throw Return(valueObj)
      case Var(name, initializer) =>
        val value = if initializer != null then evaluate(initializer) else null
        env.define(name, value)
      case While(condition, body) =>
        @tailrec
        def aux: Unit =
          if isTruthy(evaluate(condition)) then
            execute(body)
            aux
        aux

  def evaluate(expr: Expr): Any =
    expr match
      case Assign(name, value) =>
        val obj = evaluate(value)
        val distance = locals.get(expr)
        if distance != null then env.assignAt(distance, name, obj)
        else globals.assign(name, obj)
        obj
      case Binary(left, operator, right) =>
        val (l, r) = (evaluate(left), evaluate(right))
        operator.tokenType match
          case MINUS =>
            checkNumOperands(operator, l, r)
            l.asInstanceOf[Double] - r.asInstanceOf[Double]
          case PLUS =>
            if (l.isInstanceOf[Double] && r.isInstanceOf[Double])
            then l.asInstanceOf[Double] + r.asInstanceOf[Double]
            else if l.isInstanceOf[String] && r.isInstanceOf[String]
            then l.asInstanceOf[String] + r.asInstanceOf[String]
            else
              throw RuntimeError(
                operator,
                "Operands must be two numbers or two strings.",
              )
          case SLASH =>
            checkNumOperands(operator, l, r)
            l.asInstanceOf[Double] / r.asInstanceOf[Double]
          case STAR =>
            checkNumOperands(operator, l, r)
            l.asInstanceOf[Double] * r.asInstanceOf[Double]
          case GREATER =>
            checkNumOperands(operator, l, r)
            l.asInstanceOf[Double] > r.asInstanceOf[Double]
          case GREATER_EQUAL =>
            checkNumOperands(operator, l, r)
            l.asInstanceOf[Double] >= r.asInstanceOf[Double]
          case LESS =>
            checkNumOperands(operator, l, r)
            l.asInstanceOf[Double] < r.asInstanceOf[Double]
          case LESS_EQUAL =>
            checkNumOperands(operator, l, r)
            l.asInstanceOf[Double] <= r.asInstanceOf[Double]
          case BANG_EQUAL => !isEqual(l, r)
          case EQUAL_EQUAL => isEqual(l, r)
          case _ => throw RuntimeError(operator, "Invalid operator.")
      case Call(callee, paren, arguments) =>
        val calleeObj = evaluate(callee)
        val args = arguments.map(arg => evaluate(arg))
        if !calleeObj.isInstanceOf[LoxCallable] then
          throw RuntimeError(paren, "Can only call functions and classes.")
        val function = calleeObj.asInstanceOf[LoxCallable]
        if function.arity != args.length then
          throw RuntimeError(
            paren,
            "Expected " +
              function.arity + " arguments but got " +
              arguments.length + ".",
          )
        function.call(this, args)
      case Grouping(expression) => evaluate(expression)
      case Literal(value) => value
      case Logical(left, operator, right) =>
        val l = evaluate(left)
        operator.tokenType match
          case OR => if isTruthy(l) then l else evaluate(right)
          case AND => if !isTruthy(l) then l else evaluate(right)
          case _ => throw RuntimeError(operator, "Invalid operator.")
      case Unary(operator, right) =>
        val r = evaluate(right)
        operator.tokenType match
          case BANG => !isTruthy(r)
          case MINUS =>
            checkNumOperand(operator, r)
            -r.asInstanceOf[Double]
          case _ => throw RuntimeError(operator, "Invalid operator.")
      case Variable(name) =>
        lookUpVariable(name, expr)
}
