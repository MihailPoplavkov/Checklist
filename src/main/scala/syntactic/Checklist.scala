package syntactic

import com.poplavkov.lexical.Parameter

case class Checklist(title: String, expressions: Seq[Expression], allVars: Set[Parameter])
