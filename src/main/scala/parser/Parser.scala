package parser

import atto._
import Atto._
import scalaz._
import Scalaz._
import annotation.tailrec

object Parser {
    def symbol = oneOf("!#$%&|*+-/:<=>?@^_~")

    def radixPrefix = for {
        _ ← Atto.char('#')
        x ← oneOf("bodx")
    } yield x match {
        case 'b' ⇒ NumericType.BINARY
        case 'o' ⇒ NumericType.OCTAL
        case 'd' ⇒ NumericType.DECIMAL
        case 'x' ⇒ NumericType.HEXADECIMAL
    }

    def escapeChar = for {
        _ ← Atto.char('\\')
        x ← oneOf("\"\\nrt")
    } yield x match {
        case '\"' ⇒ '\"'
        case '\\' ⇒ '\\'
        case 'n' ⇒ '\n'
        case 'r' ⇒ '\r'
        case 't' ⇒ '\t'
    }

    def r5rs = escapeChar | noneOf("\"\\\n\r\t")

    def parseString: Parser[LispVal] = for {
        _ ← Atto.char('"')
        x ← many(r5rs)
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

    def parsePrefixNumber =
        radixPrefix flatMap {
            _ match {
                case NumericType.BINARY ⇒ many1(binaryDigit) map { bd ⇒
                    NumberLV(parseBinary(bd))
                }
                case NumericType.OCTAL ⇒ many1(octalDigit) map { od ⇒
                    NumberLV(parseOctal(od))
                }
                case NumericType.DECIMAL ⇒ many1(digit) map { dd ⇒
                    NumberLV(parseDecimal(dd))
                }
                case NumericType.HEXADECIMAL ⇒ many1(hexDigit) map { hd ⇒
                    NumberLV(parseHex(hd))
                }
            }
        }

    private def parseBinary(x: NonEmptyList[Char]): Integer =
        Integer.parseInt(x.toList.mkString, 2)

    private def parseOctal(x: NonEmptyList[Char]): Integer =
        Integer.parseInt(x.toList.mkString, 8)
    private def parseDecimal(x: NonEmptyList[Char]): Integer =
        Integer.parseInt(x.toList.mkString, 10)
    private def parseHex(x: NonEmptyList[Char]): Integer =
        Integer.parseInt(x.toList.mkString, 16)

    def parseRawNumber: Parser[LispVal] =
        many1(digit).map { n ⇒ NumberLV(Integer.parseInt(n.toList.mkString)) }

    def parseNumber = parsePrefixNumber | parseRawNumber

    def parseExpr = parseNumber | parseAtom | parseString

    def readExpr(expr: String): String = {
        parseExpr.parseOnly(expr).either match {
            case -\/(l) ⇒ "No match: " ++ l
            case \/-(v) ⇒ "Found: " ++ v.shows
        }
    }

}

