package com.poplavkov.output

import java.io.FileOutputStream

import com.poplavkov.syntactic.{Checklist, Expression}

abstract class Printer[T] {

  protected val params: Map[String, String]

  protected def handleTitle(title: String): T

  protected def handle(expression: Expression): T

  protected def convertToString: T => String

  def makeString(checklist: Checklist): String =
    checklist match {
      case Checklist(title, expressions, _) =>
        val t = convertToString(handleTitle(title))
        expressions.foldLeft(t) {
          case (acc, elem) =>
            acc + convertToString(handle(elem))
        }
    }

  def printToFile(checklist: Checklist, fileName: String): Unit = {
    val fos = new FileOutputStream(fileName)
    fos.write(makeString(checklist).getBytes)
    fos.close()
  }

}
