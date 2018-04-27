package com.poplavkov.lexical

import com.poplavkov.lexical.Typ.Typ

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class LexicalParser extends RegexParsers {

  //no whitespace should be handled automatically
  override val whiteSpace: Regex = "".r

  // represents the current indent. Uses to define start/end of a scope
  private var indent: String = ""
  private var indentInc: String = " "

  private def incrementIndent(): Unit = indent += indentInc

  private def toEndOfLine: Parser[String] = "[^\n]+".r

  private def word: Parser[String] = "\\w+".r

  private def title: Parser[Title] = "## " ~> toEndOfLine <~ "\n" ^^ Title

  private def header: Parser[Header] = "# " ~> "[^\n]+".r <~ "\n" ^^ Header

  private def wrapped[T](parser: Parser[T]): Parser[T] = "(" ~> parser <~ ")"

  private def functionArgument: Parser[FunctionArgument] = word ^^ FunctionArgument

  private def functionStart: Parser[FunctionStart] = "$$" ~> word ~ wrapped(repsep(functionArgument, ",")) ^^ {
    case name ~ list =>
      incrementIndent()
      FunctionStart(name, list)
  }

  private def typ: Parser[Typ] = Typ.values.map(v => v.toString ^^^ v).reduce(_|_)

}
