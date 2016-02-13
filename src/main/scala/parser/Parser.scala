package parser

import atto._
import Atto._
import scalaz._
import Scalaz._

object Parser {
  def symbol = oneOf("!#$%&|*+-/:<=>?@^_~")

  def readExpr(expr: String): String = {
    symbol.parse(expr).either match {
      case -\/(l) => "No match: " ++ l
      case \/-(v) => "Found: " ++ v.toString
    }
  }

}

