package com.poplavkov.lexical

import com.poplavkov.lexical.LexicalParser._
import org.scalatest.{FlatSpec, Matchers}

class LexicalParserTest extends FlatSpec with Matchers {

  "title parser" should "parse a title" in {
    val input = "Список в поездку"
    val result = LexicalParser.title.parse(s"## $input\n")
    assert(result.successful)
    result.get shouldBe input
  }

  it should "not parse a string with incorrect title" in {
    Seq(
      "##title\n",
      "# title\n",
      "title\n",
      "## \n") foreach { str =>
      assert(LexicalParser.title.parse(str).isEmpty)
    }
  }

  "function parser" should "parse a simple function" in {
    val input = "$$Переходник(страна)\n Переходник на $страна розетки"
    val name = "Переходник"
    val args = Seq("страна")
    val body = Seq(Line(Seq(Part("Переходник на "), SimpleArgument("страна"), Part(" розетки"))))

    val result = LexicalParser.function.parse(input)
    assert(result.successful)
    result.get shouldBe Function(FunctionHeader(name, args), body)
  }

  it should "parse a function with many args and many lines" in {
    val input =
      """$$Function(arg_1, arg_2, arg_3)
        | body1
        | body2
        | body3 with $arg_1 and $function_call(arg1, arg2)""".stripMargin
    val name = "Function"
    val args = Seq("arg_1", "arg_2", "arg_3")
    val body = Seq(
      Line(Seq(Part("body1"))),
      Line(Seq(Part("body2"))),
      Line(Seq(Part("body3 with "), SimpleArgument("arg_1"), Part(" and "), FunctionCall("function_call", Seq("arg1", "arg2")))))

    val result = LexicalParser.function.parse(input)
    assert(result.successful)
    result.get shouldBe Function(FunctionHeader(name, args), body)
  }

  "scope parser" should "parse a simple scope" in {
    val input =
      "-> Количество дней: Число: дни\n # Одежда"
    val params = Seq(Parameter(Typ.Num, "дни", "Количество дней"))
    val body = Seq(Header("Одежда"))

    val result = LexicalParser.scope.parse(input)
    assert(result.successful)
    result.get shouldBe Scope(params, body)
  }

  it should "parse a complex scope" in {
    val input =
      """-> Количество дней: Число: дни, Месяц: Строка: месяц
        | # Одежда
        | -> Еще один: Число: опа
        |  # Обувь""".stripMargin
    val params = Seq(
      Parameter(Typ.Num, "дни", "Количество дней"),
      Parameter(Typ.Str, "месяц", "Месяц"))
    val body = Seq(
      Header("Одежда"),
      Scope(Seq(Parameter(Typ.Num, "опа", "Еще один")), Seq(Header("Обувь"))))

    val result = LexicalParser.scope.parse(input)
    assert(result.successful)
    result.get shouldBe Scope(params, body)
  }

  "LexicalParser" should "parse a string with only a title" in {
    val title = "Список в поездку"
    val result = LexicalParser.parseChecklist(s"## $title")
    assert(result.successful)
    result.get shouldBe Checklist(title, Seq.empty)
  }

  it should "parse a checklist with two headers" in {
    val input =
      """## TITLE
        |# Header 1
        |# Header 2""".stripMargin

    val expected =
      Checklist(
        "TITLE",
        Seq(
          Header("Header 1"),
          Header("Header 2")))

    val result = LexicalParser.parseChecklist(input)

    assert(result.successful)
    result.get shouldBe expected
  }

  it should "parse a checklist with function" in {
    val input =
      """## TITLE
        |$$function(arg1, arg2)
        | function
        | body + $arg1$arg2
        | end""".stripMargin

    val expected =
      Checklist(
        "TITLE",
        Seq(
          Function(
            FunctionHeader("function", Seq("arg1", "arg2")),
            Seq(
              Line(Seq(Part("function"))),
              Line(Seq(Part("body + "), SimpleArgument("arg1"), SimpleArgument("arg2"))),
              Line(Seq(Part("end")))))))

    val result = LexicalParser.parseChecklist(input)

    assert(result.successful)
    result.get shouldBe expected
  }

  it should "parse a complex checklist" in {
    val input =
      """## TITLE
        |# Header 1
        |$$function(arg1, arg2)
        | function
        | body + $arg1$arg2
        | end
        |-> Scope: Строка: scope
        | $function(scope)
        | and one more scope
        | -> One more scope: Число: new_scope
        |  body
        |  of
        |  the
        |  second
        |  scope
        |# Header 2
        |the end""".stripMargin

    val expected =
      Checklist(
        "TITLE",
        Seq(
          Header("Header 1"),
          Function(
            FunctionHeader("function", Seq("arg1", "arg2")),
            Seq(
              Line(Seq(Part("function"))),
              Line(Seq(Part("body + "), SimpleArgument("arg1"), SimpleArgument("arg2"))),
              Line(Seq(Part("end"))))),
          Scope(
            Seq(Parameter(Typ.Str, "scope", "Scope")),
            Seq(
              Line(Seq(FunctionCall("function", Seq("scope")))),
              Line(Seq(Part("and one more scope"))),
              Scope(
                Seq(Parameter(Typ.Num, "new_scope", "One more scope")),
                Seq(
                  Line(Seq(Part("body"))),
                  Line(Seq(Part("of"))),
                  Line(Seq(Part("the"))),
                  Line(Seq(Part("second"))),
                  Line(Seq(Part("scope"))))))),
          Header("Header 2"),
          Line(Seq(Part("the end")))))

    val result = LexicalParser.parseChecklist(input)

    assert(result.successful)
    result.get shouldBe expected
  }

  private def parseWith[T](parser: Parser[T], input: String): ParseResult[T] = {
    parseAll(parser, input)
  }

  private implicit class ParserOps[T](parser: Parser[T]) {
    def parse(input: String): ParseResult[T] = parseWith(parser, input)
  }

}
