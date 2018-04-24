package com.poplavkov.lexical

import org.scalatest.FlatSpec

class TypeTest extends FlatSpec {
  "Number creation" should "fail by requre error" in {
     assertThrows[IllegalArgumentException](Number("abc"))
     assertThrows[IllegalArgumentException](Number("12a3"))
     assertThrows[IllegalArgumentException](Number("123."))
     assertThrows[IllegalArgumentException](Number(".123"))
     assertThrows[IllegalArgumentException](Number("."))
  }
}
