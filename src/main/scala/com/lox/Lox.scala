package com.lox

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset
import java.io.InputStreamReader
import java.io.BufferedReader
import scala.annotation.tailrec

trait Lox {
  def start(args: List[String]): Unit
  def error(line: Int, msg: String): Unit
  def error(t: Token, msg: String): Unit
  def runtimeError(e: RuntimeError): Unit
}

def Lox: Lox =
  new Lox {
    val interpreter = Interpreter

    var hadError = false
    var hadRuntimeError = false

    override def start(args: List[String]): Unit =
      if args.length > 1 then
        println("Usage: jlox [script]")
        System.exit(64)
      else if args.length == 1 then runFile(args(0))
      else runPrompt

    override def error(line: Int, msg: String): Unit =
      report(line, "", msg)

    override def error(t: Token, msg: String): Unit =
      if t.tokenType == TokenType.EOF then report(t.line, " at end", msg)
      else report(t.line, " at '" + t.lexeme + "'", msg)

    override def runtimeError(e: RuntimeError): Unit =
      println(e.s + "\n[line " + e.t.line + "]")
      hadRuntimeError = true

    def report(line: Int, where: String, msg: String) =
      System.err.println("[line " + line + "] Error" + where + ": " + msg)
      hadError = true

    def runFile(path: String) =
      val bytes = Files.readAllBytes(Paths.get(path))
      run(new String(bytes, Charset.defaultCharset()))
      if hadError then System.exit(65)
      if hadRuntimeError then System.exit(70)

    def runPrompt =
      val input = new InputStreamReader(System.in)
      val reader = new BufferedReader(input)
      @tailrec
      def loop: Unit =
        print("> ")
        val line = reader.readLine
        if line != null then
          run(line)
          hadError = false
          loop
      loop

    def run(source: String) =
      val scanner = Scanner(source)
      val tokens = scanner.scanTokens
      val parser = Parser(tokens)
      val stmts = parser.parse
      if !hadError then
        val resolver = Resolver(interpreter)
        resolver.resolve(stmts)
        if !hadError then interpreter.interpret(stmts)
  }
