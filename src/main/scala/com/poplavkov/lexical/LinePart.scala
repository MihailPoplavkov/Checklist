package com.poplavkov.lexical

sealed trait LinePart

sealed trait Argument extends LinePart

case class SimpleArgument(name: String) extends Argument

case class FunctionCall(name: String, args: Seq[String]) extends Argument

case class Part(value: String) extends LinePart
