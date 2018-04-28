package syntactic

sealed trait StrOrVar

case class Str(value: String) extends StrOrVar

case class Var(value: String) extends StrOrVar
