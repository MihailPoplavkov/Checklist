package com.poplavkov.lexical

sealed trait Token

case class Title(value: String) extends Token

case class Header(value: String) extends Token

case class Line(parts: Seq[String], args: Seq[String]) extends Token {
  private val diff = parts.size - args.size
  require(diff == 0 || diff == 1)
}

case class FunctionStart(name: String, args: Argument*) extends Token

case class FunctionEnd() extends Token

case class ScopeStart(params: Parameter*) extends Token

case class ScopeEnd() extends Token

sealed trait IfExpression extends Token

case class SingleIfExpression(argument: Argument, operator: Operator, value: Typ) extends IfExpression

case class CombinedIfExpression(expressions: SingleIfExpression*) extends IfExpression
