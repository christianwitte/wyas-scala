package parser

import atto._
import Atto._
import scalaz._
//import Scalaz._

object Parser {
    def symbol = oneOf("!#$%&|*+-/:<=>?@^_~")

    def parseString: Parser[LispVal] = for {
        _ ← char('"')
        x ← many(noneOf("\\"))
        _ ← char('"')
    } yield (StringLV(x.mkString))

    def parseAtom: Parser[LispVal] = for {
        first ← letter | symbol
        rest ← many(letter | digit | symbol)
        atom = first +: rest
    } yield atom.mkString match {
        case "#t" ⇒ BoolLV(true)
        case "#f" ⇒ BoolLV(false)
        case a ⇒ Atom(a)
    }

    def readExpr(expr: String): String = {
        symbol.parse(expr).either match {
            case -\/(l) ⇒ "No match: " ++ l
            case \/-(v) ⇒ "Found: " ++ v.toString
        }
    }

}

