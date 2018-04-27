package com.poplavkov.lexical

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class TypeTest extends FlatSpec {
  "Number creation" should "fail by require error" in {
    assertThrows[IllegalArgumentException](Number("abc"))
    assertThrows[IllegalArgumentException](Number("12a3"))
    assertThrows[IllegalArgumentException](Number("123."))
    assertThrows[IllegalArgumentException](Number(".123"))
    assertThrows[IllegalArgumentException](Number("."))
  }

  "Number creation" should "pass through correct numbers" in {
    Number("123").value shouldBe "123"
    Number("123.5").value shouldBe "123.5"
    Number("1.235").value shouldBe "1.235"
  }
}
