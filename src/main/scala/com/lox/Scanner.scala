package com.lox

import TokenType.*
import scala.annotation.tailrec

trait Scanner(s: String) {
  def scanTokens: List[Token]
}

def Scanner(s: String): Scanner = new Scanner(s: String) {
  val source = s

  var tokens = List[Token]()
  var start = 0
  var current = 0
  var line = 1

  override def scanTokens: List[Token] =
    @tailrec
    def aux: Unit =
      if !isAtEnd then
        start = current
        scanToken
        aux
    aux
    tokens = Token(EOF, "", null, line, current) :: tokens
    tokens.reverse

  def isAtEnd =
    current >= source.length

  def advance =
    val c = source.charAt(current)
    current = current + 1
    c

  def scanToken =
    val c = advance
    val isDigit = "([0-9])".r
    val isAlpha = "([a-zA-Z])".r
    c match
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' =>
        addToken(if matchTT('=') then BANG_EQUAL else BANG)
      case '=' =>
        addToken(if matchTT('=') then EQUAL_EQUAL else EQUAL)
      case '<' =>
        addToken(if matchTT('=') then LESS_EQUAL else LESS)
      case '>' =>
        addToken(if matchTT('=') then GREATER_EQUAL else GREATER)
      case '/' =>
        if matchTT('/') then advanceNewline
        else addToken(SLASH)
      case ' ' | '\r' | '\t' => ()
      case '\n' => line = line + 1
      case '"' => string
      case isDigit(d) => number
      case 'o' => if matchTT('r') then addToken(OR)
      case isAlpha(a) => identifier
      case _ =>
        Lox.error(line, "Unexpected character.")

  def advanceNewline =
    @tailrec
    def aux: Unit =
      if peek != '\n' && !isAtEnd then
        advance
        aux
    aux

  def peek =
    if isAtEnd then '\u0000'
    else source.charAt(current)

  def peekNext =
    if current + 1 >= source.length then '\u0000'
    else source.charAt(current + 1)

  def addToken(t: TokenType, literal: Any = null) =
    val text = source.substring(start, current)
    tokens = Token(t, text, literal, line, current) :: tokens

  def matchTT(c: Char) =
    if isAtEnd then false
    else if source.charAt(current) != c then false
    else
      current = current + 1
      true

  def string =
    @tailrec
    def auxAdv: Unit =
      if peek != '"' && !isAtEnd then
        if peek == '\n' then line = line + 1
        advance
        auxAdv
    auxAdv
    if isAtEnd then Lox.error(line, "Unterminated string.")
    else
      advance
      val value = source.substring(start + 1, current - 1)
      addToken(STRING, value)

  def number =
    @tailrec
    def auxNum: Unit =
      if peek.isDigit then
        advance
        auxNum
    auxNum
    if peek == '.' && peekNext.isDigit then
      advance
      auxNum
    addToken(NUMBER, source.substring(start, current).toDouble)

  def identifier =
    @tailrec
    def auxAN: Unit =
      if peek.isLetterOrDigit then
        advance
        auxAN
    auxAN
    val text = source.substring(start, current)
    val tokenType =
      text match
        case "and" => AND
        case "class" => CLASS
        case "else" => ELSE
        case "false" => FALSE
        case "for" => FOR
        case "fun" => FUN
        case "if" => IF
        case "nil" => NIL
        case "or" => OR
        case "print" => PRINT
        case "return" => RETURN
        case "super" => SUPER
        case "this" => THIS
        case "true" => TRUE
        case "var" => VAR
        case "while" => WHILE
        case _ => IDENTIFIER
    addToken(tokenType)
}
