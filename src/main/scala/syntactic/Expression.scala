package syntactic

sealed trait Expression

case class HeaderExpr(value: String) extends Expression

case class LineExpr(seq: Seq[StrOrVar]) extends Expression

case class IncrementIndent() extends Expression

case class DecrementIndent() extends Expression
