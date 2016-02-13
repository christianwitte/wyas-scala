import parser.Parser
import scala.io.StdIn.readLine

object WYAS extends App {
    println("Write yourself a scheme...")
    val expr = readLine("Please enter an expression: ")
    val v = Parser.readExpr(expr)
    println(v)
}
