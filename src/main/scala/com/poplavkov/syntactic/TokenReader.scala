package com.poplavkov.syntactic

import com.poplavkov.lexical.Token

import scala.util.parsing.input.{NoPosition, Position, Reader}

class TokenReader[T <: Token](tokens: Seq[T]) extends Reader[T] {

  def first: T = tokens.head

  def atEnd: Boolean = tokens.isEmpty

  def pos: Position = NoPosition

  def rest: Reader[T] = new TokenReader(tokens.tail)

}
