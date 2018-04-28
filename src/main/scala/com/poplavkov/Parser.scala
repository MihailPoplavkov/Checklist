package com.poplavkov

import java.io.File
import java.nio.file.Files

import com.poplavkov.lexical.LexicalParser._
import com.poplavkov.lexical.{LexicalParser, Typ}
import com.poplavkov.output.StringPrinter
import com.poplavkov.syntactic.{Checklist, SyntacticParser}

import scala.collection.JavaConverters._
import scala.io.StdIn
import scala.util.Try

object Parser {

  def parseInput(input: String): Either[String, Checklist] = {
    LexicalParser.parseChecklist(input) match {
      case NoSuccess(msg, next) =>
        Left(s"$msg at line ${next.pos.line}")
      case Success(structure, _) =>
        Try(SyntacticParser.parseStructure(structure)).toEither.left.map(_.getMessage)
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Please specify arguments (method and path to template)")
      return
    }
    val outputFile: Option[String] = args.head match {
      case "--interactive" =>
        None
      case s if s.startsWith("--file:") && s.length > 7 =>
        Some(s.substring(7))
      case _ =>
        println("Please specify method (one of --interactive or --file)")
        return
    }
    val checklistFile = args(1)

    val input = Files.readAllLines(new File(checklistFile).toPath).asScala.mkString("\n")
    parseInput(input) match {
      case Left(err) =>
        println("ERROR occurred:")
        println(err)
      case Right(checklist) =>
        val params = checklist.allVars.map { par =>
          println(s"Please specify ${par.description} (${par.typ})")
          val s = par.typ match {
            case Typ.Num =>
              var str = StdIn.readLine()
              while(!str.matches("\\d+(\\.\\d+)?")) {
                println("Bad number format. Again")
                str = StdIn.readLine()
              }
              str
            case Typ.Str =>
              StdIn.readLine()
          }
          par.name -> s.toString
        }
        StdIn.readLine("Please specify output format (for now string is the only format, so press enter)")
        val printer = new StringPrinter(params.toMap)
        outputFile match {
          case None => println(printer.makeString(checklist))
          case Some(filename) => printer.printToFile(checklist, filename)
        }
    }
    println("Bye")
  }
}
