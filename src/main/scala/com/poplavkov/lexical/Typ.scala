package com.poplavkov.lexical

sealed trait Typ {
  def value: String
}

case class Number(override val value: String) extends Typ {
  private val regex = "%d+((\\.%d+)?)"
  require(value.matches(regex))
}

case class Str(override val value: String) extends Typ

