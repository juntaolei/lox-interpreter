package com.lox

case class Token(
    tokenType: TokenType,
    lexeme: String,
    literal: Any,
    line: Int,
    pos: Int,
)
