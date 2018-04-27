package com.poplavkov.lexical

object Typ extends Enumeration {
  type Typ = Value

  val Str: Typ = Value("Строка")
  val Num: Typ = Value("Число")
}
