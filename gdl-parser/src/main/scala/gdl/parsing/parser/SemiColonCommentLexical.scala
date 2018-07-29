package gdl.parsing.parser

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

class SemiColonCommentLexical extends StdLexical {
  override def whitespace: Parser[Any] = rep[Any](
    whitespaceChar | ';' ~ rep( chrExcept(EofCh, '\n') )
  )
}
