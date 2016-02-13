package parser

import scalaz.Show
import scalaz._
import Scalaz._

sealed trait LispVal

case class Atom(a: String) extends LispVal
case class ListLV(l: List[LispVal]) extends LispVal
case class DottedList(l: List[LispVal], v: LispVal) extends LispVal
case class NumberLV(n: Integer) extends LispVal
case class StringLV(s: String) extends LispVal
case class BoolLV(b: Boolean) extends LispVal

object LispVal {
  implicit def LispValShow: Show[LispVal] = new Show[LispVal] {
    override def show(lv: LispVal) = lv match {
      case Atom(a) => a
      case NumberLV(n) => n.toString()
      case StringLV(s) => s
      case BoolLV(b) => b.show
    }
  }

}
