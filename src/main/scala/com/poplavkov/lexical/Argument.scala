package com.poplavkov.lexical

sealed trait Argument

case class IntValue(value: Int)

case class Simple(name: String)

case class FunctionCall(name: String, args: String*)

// returns int
case class Expression(operands: Seq[Argument], operators: Seq[Operator])
