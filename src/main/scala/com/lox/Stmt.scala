package com.lox

enum Stmt:
  case Block(statements: List[Stmt])
  case Expression(expression: Expr)
  case Function(name: Token, params: List[Token], body: List[Stmt])
  case If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt)
  case Print(expression: Expr)
  case Return(keyword: Token, value: Expr)
  case Var(name: Token, initializer: Expr)
  case While(condition: Expr, body: Stmt)
