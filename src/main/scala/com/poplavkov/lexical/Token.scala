package com.poplavkov.lexical

sealed trait Token

case class Header(value: String) extends Token

case class Line(elems: Seq[LinePart]) extends Token

case class Function(header: FunctionHeader, body: Seq[Line]) extends Token

case class Scope(params: Seq[Parameter], body: List[Token]) extends Token
