package com.lox

import Expr.*
import TokenType.*
import Stmt.*
import scala.annotation.tailrec
import scala.annotation.static

trait Parser(t: List[Token]) {
  type ParserError

  def parse: List[Stmt]
}

def Parser(t: List[Token]): Parser = new Parser(t: List[Token]) {
  var current = 0
  var tokens = t

  case class ParserError() extends RuntimeException

  override def parse: List[Stmt] =
    @tailrec
    def aux(lst: List[Stmt]): List[Stmt] =
      if !isAtEnd then aux(declaration :: lst)
      else lst
    aux(Nil).reverse

  def declaration: Stmt =
    try
      if matchTT(FUN) then function("function")
      else if matchTT(VAR) then varDeclaration
      else statement
    catch
      case ParserError() =>
        synchronize
        null

  def function(kind: String) =
    val name = consume(IDENTIFIER, "Expect " + kind + " name.")
    consume(LEFT_PAREN, "Expect '(' after " + kind + " name.")
    @tailrec
    def aux(lst: List[Token]): List[Token] =
      if lst.length >= 255 then
        error(peek, "Can't have more than 255 parameters.")
      val nlst = consume(IDENTIFIER, "Expect parameter name.") :: lst
      if matchTT(COMMA) then aux(nlst)
      else nlst
    val parameters = if !check(RIGHT_PAREN) then aux(Nil).reverse else Nil
    consume(RIGHT_PAREN, "Expect ')' after parameters.")
    consume(LEFT_BRACE, "Expect '{' before " + kind + " body.")
    val body = block
    Stmt.Function(name, parameters, body)

  def varDeclaration =
    val name = consume(IDENTIFIER, "Expect variable name.")
    val expr = if matchTT(EQUAL) then expression else null
    consume(SEMICOLON, "Expect ';' after variable declaration.")
    Var(name, expr)

  def statement: Stmt =
    if matchTT(FOR) then forStatement
    else if matchTT(IF) then ifStatement
    else if matchTT(PRINT) then printStatement
    else if matchTT(RETURN) then returnStatement
    else if matchTT(WHILE) then whileStatement
    else if matchTT(LEFT_BRACE) then Block(block)
    else expressionStatement

  def forStatement =
    consume(LEFT_PAREN, "Expect '(' after 'for'.")
    val initializer =
      if matchTT(SEMICOLON) then null
      else if matchTT(VAR) then varDeclaration
      else expressionStatement
    val condition = if !check(SEMICOLON) then expression else Literal(true)
    consume(SEMICOLON, "Expect ';' after loop condition.")
    val increment = if !check(RIGHT_PAREN) then expression else null
    consume(RIGHT_PAREN, "Expect ')' after for clauses.")
    val body =
      val t =
        if increment != null then
          Block(statement :: List(Expression(increment)))
        else statement
      if initializer != null then
        Block(initializer :: List(While(condition, t)))
      else While(condition, t)
    body

  def ifStatement =
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression
    consume(RIGHT_PAREN, "Expect ')' after if condition.")
    val thenBranch = statement
    val elseBranch = if matchTT(ELSE) then statement else null
    If(condition, thenBranch, elseBranch)

  def printStatement =
    val expr = expression
    consume(SEMICOLON, "Expect ';' after value.")
    Print(expr)

  def returnStatement =
    val keyword = previous
    val value = if !check(SEMICOLON) then expression else null
    consume(SEMICOLON, "Expect ';' after return value.")
    Return(keyword, value)

  def whileStatement =
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition = expression
    consume(RIGHT_PAREN, "Expect ')' after condition.")
    val body = statement
    While(condition, body)

  def block =
    @tailrec
    def aux(lst: List[Stmt]): List[Stmt] =
      if !check(RIGHT_BRACE) && !isAtEnd then aux(declaration :: lst)
      else lst
    val statements = aux(Nil).reverse
    consume(RIGHT_BRACE, "Expect '}' after block.")
    statements

  def expressionStatement =
    val expr = expression
    consume(SEMICOLON, "Expect ';' after expression.")
    Expression(expr)

  def expression =
    assignment

  def assignment: Expr =
    val expr = or
    if matchTT(EQUAL) then
      val equals = previous
      val value = assignment
      expr match
        case Variable(name) => Assign(name, value)
        case _ => error(equals, "Invalid assignment target.")
    else expr

  @tailrec
  def makeExpr(
      f: () => Expr,
      expr: Expr,
      et: (Expr, Token, Expr) => Expr,
      tt: TokenType*
  ): Expr =
    if matchTT(tt*) then
      val operator = previous
      val right = f()
      makeExpr(f, et(expr, operator, right), et, tt*)
    else expr

  def makeBinary(f: () => Expr, expr: Expr, tt: TokenType*) =
    makeExpr(f, expr, (l, o, r) => Binary(l, o, r), tt*)

  def makeLogical(f: () => Expr, expr: Expr, tt: TokenType*) =
    makeExpr(f, expr, (l, o, r) => Logical(l, o, r), tt*)

  def or =
    makeLogical(() => and, and, OR)

  def and =
    makeLogical(() => equality, equality, AND)

  def equality =
    makeBinary(() => comparison, comparison, BANG_EQUAL, EQUAL_EQUAL)

  def comparison =
    makeBinary(() => term, term, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)

  def term =
    makeBinary(() => factor, factor, PLUS, MINUS)

  def factor =
    makeBinary(() => unary, unary, SLASH, STAR)

  def unary: Expr =
    if matchTT(BANG, MINUS) then
      val operator = previous
      val right = unary
      Unary(operator, right)
    else call

  def call =
    val expr = primary
    @tailrec
    def aux(expr: Expr): Expr =
      if matchTT(LEFT_PAREN) then aux(finishCall(expr))
      else expr
    aux(expr)

  def finishCall(callee: Expr) =
    @tailrec
    def aux(lst: List[Expr]): List[Expr] =
      if lst.length >= 255 then
        error(peek, "Can't have more than 255 arguments.")
      val nlst = expression :: lst
      if matchTT(COMMA) then aux(nlst)
      else nlst
    val arguments = if !check(RIGHT_PAREN) then aux(Nil).reverse else Nil
    val paren = consume(RIGHT_PAREN, "Expect ')' after arguments.")
    Call(callee, paren, arguments)

  def primary: Expr =
    if matchTT(FALSE) then Literal(false)
    else if matchTT(TRUE) then Literal(true)
    else if matchTT(NIL) then Literal(null)
    else if matchTT(NUMBER, STRING) then Literal(previous.literal)
    else if matchTT(LEFT_PAREN) then
      val expr = expression
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      Grouping(expr)
    else if matchTT(IDENTIFIER) then Variable(previous)
    else throw error(peek, "Expect expression.")

  def consume(t: TokenType, msg: String) =
    if check(t) then advance
    else throw error(peek, msg)

  def error(t: Token, msg: String) =
    Lox.error(t, msg)
    throw ParserError()

  def matchTT(tt: TokenType*) =
    @tailrec
    def aux(tt: List[TokenType]): Boolean =
      tt match
        case Nil => false
        case head :: next =>
          if check(head) then
            advance; true
          else aux(next)
    aux(tt.toList)

  def synchronize =
    advance
    @tailrec
    def aux: Unit =
      if !isAtEnd && previous.tokenType != SEMICOLON then
        peek.tokenType match
          case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => ()
          case _ => advance; aux
    aux

  def isAtEnd =
    peek.tokenType == EOF

  def peek =
    tokens(current)

  def previous =
    tokens(current - 1)

  def advance =
    if !isAtEnd then current = current + 1
    previous

  def check(t: TokenType) =
    if isAtEnd then false
    else peek.tokenType == t
}
