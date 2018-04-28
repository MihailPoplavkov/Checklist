package com.poplavkov.lexical

import com.poplavkov.lexical.Typ.Typ

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object LexicalParser extends RegexParsers {

  //no whitespace should be handled automatically
  override val whiteSpace: Regex = "".r

  // represents the current indent. Uses to define start/end of a scope
  private def indent: String = " " * indentLen

  private var indentLen = 0
  private val indentInc = 1

  def parseChecklist(input: String): ParseResult[Checklist] = {
    parseAll(checklist, input + "\n")
  }

  private def incrementIndent(): Unit = indentLen += indentInc

  private def decrementIndent(): Unit = indentLen -= indentInc

  private def allExcept(exclude: Char*): Parser[String] = s"[^${exclude.mkString}]+".r

  private def word: Parser[String] = "\\w+".r

  private def title: Parser[String] = "## " ~> allExcept('\n') <~ "\n"

  private def header: Parser[Header] = "# " ~> allExcept('\n') ^^ Header

  private def wrapped[T](parser: Parser[T]): Parser[T] = "(" ~> parser <~ ")"

  private def comma: Parser[Any] = "," ~ opt(" ")

  private def functionHeader: Parser[FunctionHeader] =
    "$$" ~> word ~ wrapped(repsep(word, comma)) ^^ {
      case name ~ list =>
        incrementIndent()
        FunctionHeader(name, list)
    }

  private def function: Parser[Function] =
    functionHeader ~ rep1(indent ~> line) ^^ {
      case header ~ body =>
        decrementIndent()
        Function(header, body)
    }

  private def functionCall: Parser[FunctionCall] =
    word ~ wrapped(rep1sep(word, comma)) ^^ {
      case name ~ list => FunctionCall(name, list)
    }

  private def argument: Parser[Argument] = "$" ~> (functionCall | word ^^ SimpleArgument)

  private def line: Parser[Line] =
    rep((allExcept('\n', '$') ^^ Part) | argument) ^^ Line

  private def typ: Parser[Typ] = {
    val typesWithNames: Set[(String, Typ)] = Typ.values.map(typ => (typ.toString, typ))
    typesWithNames.map {
      case (str, typ) => str ^^^ typ
    } reduce (_ | _)
  }

  private def parameter: Parser[Parameter] =
    ("-> " ~> allExcept(':') <~ ": ") ~ (typ <~ ": ") ~ word ^^ {
      case description ~ typ ~ name => Parameter(typ, name, description)
    }

  private def parameters: Parser[List[Parameter]] =
    rep1sep(parameter, comma) ^^ { params =>
      incrementIndent()
      params
    }

  private def scope: Parser[Scope] =
    parameters ~ rep1(token) ^^ {
      case params ~ tokens =>
        decrementIndent()
        Scope(params, tokens)
    }

  private def token: Parser[Token] = indent ~> (header | function | scope | line) <~ "\n"

  private def checklist: Parser[Checklist] =
    title ~ rep(token) ^^ {
      case title ~ tokens => Checklist(title, tokens)
    }

}
