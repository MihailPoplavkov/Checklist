package com.poplavkov.output

import com.poplavkov.syntactic._

class StringPrinter(override val params: Map[String, String]) extends Printer[String] {
  private var indentLen = 0
  private def indent: String = " " * indentLen

  protected def handleTitle(title: String): String = title.toUpperCase + "\n\n"

  protected def handle(expression: Expression): String = expression match {
    case HeaderExpr(value) => indent + value + "\n"
    case IncrementIndent() =>
      indentLen += 1
      ""
    case DecrementIndent() =>
      indentLen -= 1
      ""
    case LineExpr(seq) =>
      val str = seq.map {
        case Str(s) => s
        case Var(v) => params.getOrElse(v, "_none_")
      }
      indent + str.mkString + "\n"
  }

  protected def convertToString: String => String = identity
}
