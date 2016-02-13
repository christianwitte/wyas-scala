package parser

import atto._
import Atto._
import scalaz._
import Scalaz._

object Parser {
    def symbol = oneOf("!#$%&|*+-/:<=>?@^_~")

    def parseString: Parser[LispVal] = for {
        _ ← Atto.char('"')
        x ← many(noneOf("\""))
        _ ← Atto.char('"')
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

    def parseNumber: Parser[LispVal] =
        many1(digit).map { n ⇒ NumberLV(Integer.parseInt(n.toList.mkString)) }

    def parseExpr = parseAtom | parseString | parseNumber

    def readExpr(expr: String): String = {
        parseExpr.parseOnly(expr).either match {
            case -\/(l) ⇒ "No match: " ++ l
            case \/-(v) ⇒ "Found: " ++ v.toString
        }
    }

}

