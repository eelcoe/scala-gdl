package gdl.parsing.parser

import gdl.lang.Description

import scala.util.Try
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharSequenceReader

trait GdlParser extends StandardTokenParsers {
  def apply(text: String): Try[Description]

  def parseAs[T](description: String)(implicit parser: Parser[T]): Try[T] = {
    val tokenReader = new lexical.Scanner(new CharSequenceReader(description))
    parser.apply(tokenReader) match {
      case Success(result, _) => scala.util.Success(result)
      case NoSuccess(message, _) => scala.util.Failure(new ParseException(message))
    }
  }
}
