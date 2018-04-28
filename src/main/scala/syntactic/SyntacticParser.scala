package syntactic

import com.poplavkov.lexical._

import scala.util.DynamicVariable
import scala.util.parsing.combinator.Parsers

object SyntacticParser extends Parsers {

  override type Elem = Token

  private val state = new DynamicVariable[State](State())

  def parseStructure(checklistStructure: ChecklistStructure): Checklist =
    state.withValue(State()) {
      checklistStructure match {
        case ChecklistStructure(title, elems) =>
          Checklist(title, parseTokens(elems, phrase(tokens)))
      }
    }

  private def parseTokens(tokens: Seq[Token], parser: Parser[Seq[Expression]]): Seq[Expression] =
    parser(new TokenReader(tokens)) match {
      case NoSuccess(msg, next) =>
        throw new RuntimeException(s"Syntactic parser error on ${next.pos.line} with msg: $msg")
      case Success(seq, _) =>
        seq
    }

  private def tokens: Parser[Seq[Expression]] =
    rep1(opt(function) ~> (rep1(header) | rep1(line) | scope)) ^^ (_.flatten)

  private def scope: Parser[Seq[Expression]] =
    elem("Scope", _.isInstanceOf[Scope]) ^^ (_.asInstanceOf[Scope]) ^^ {
      case Scope(params, elems) =>
        if (state.value.allParams.intersect(params.toSet).nonEmpty)
          throw new RuntimeException("Some parameters already exist in the scope")
        state.value = state.value.copy(
          functionsHistory = List() :: state.value.functionsHistory,
          varsHistory = params :: state.value.varsHistory,
          allParams = state.value.allParams ++ params)
        val seq = IncrementIndent() +: parseTokens(elems, tokens) :+ DecrementIndent()
        state.value = state.value.copy(
          functionsHistory = state.value.functionsHistory.tail,
          varsHistory = state.value.varsHistory.tail)
        seq
    }

  private def function: Parser[Unit] =
    elem("Function", _.isInstanceOf[Function]) ^^ (_.asInstanceOf[Function]) ^^ { fun =>
      val hist = state.value.functionsHistory
      if (hist.flatten.exists(_.header.name == fun.header.name))
        throw new RuntimeException(s"Function '${fun.header.name}' already exists in the scope")
      val lastHistory = fun :: hist.head
      state.value = state.value.copy(functionsHistory = lastHistory :: hist.tail)
    }

  private def header: Parser[HeaderExpr] =
    elem("Header", _.isInstanceOf[Header]) ^^ (_.asInstanceOf[Header]) ^^ {
      case Header(value) =>
        HeaderExpr(value)
    }

  private def line: Parser[LineExpr] =
    elem("Line", _.isInstanceOf[Line]) ^^ (_.asInstanceOf[Line]) ^^ { line =>
      LineExpr(linePartToStrOrVar(line))
    }

  private def linePartToStrOrVar(line: Line): Seq[StrOrVar] = line match {
    case Line(parts) => parts.flatMap {
      case Part(value) =>
        Seq(Str(value))
      case SimpleArgument(name) =>
        Seq(Var(name))
      case FunctionCall(name, params) =>
        state.value.functionsHistory.flatten.find(_.header.name == name) match {
          case None =>
            throw new RuntimeException(s"No function with name '$name' found")
          case Some(Function(FunctionHeader(_, args), body)) =>
            if (params.size != args.size)
              throw new RuntimeException("Wrong parameters count")
            if (body.size != 1)
              throw new RuntimeException("Unsupported function")
            linePartToStrOrVar(body.head)
        }
    }
  }


}

case class State(
                  functionsHistory: List[List[Function]] = List(List.empty),
                  varsHistory: List[Seq[Parameter]] = List(Seq.empty),
                  allParams: Set[Parameter] = Set.empty)
