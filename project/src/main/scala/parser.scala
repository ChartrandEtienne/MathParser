package parsing
import scala.util.matching.Regex
import java.util.regex.Pattern
import scala.util.parsing.combinator.RegexParsers
import math.{pow, sqrt, cbrt}
import scala.collection.mutable.Queue


class MathLexer extends RegexParsers {
  def posNumber: Parser[String] = regex(new Regex("[0-9]+")) // " 
  def number = posNumber // | negNumber
  def variable: Parser[String] = regex(new Regex("[a-z]")) // "
  def operator: Parser[String] = regex(new Regex("[/*/^//+-]")) | "rt"  //"
  def exprLex = rep(number | variable | operator)
  def openParens: Parser[String] = "("
  def closeParens: Parser[String] = ")"
  def applyLex(input: String): List[String] = parse(exprLex, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => {println(failure.msg); Nil}
  }
}






object p extends MathLexer {
  def litteral = number ^^ {x => NumeralNode(x.toInt)}
  def negVariable = openParens ~ "-" ~ variable ~ closeParens ^^ {case op ~ min ~ x ~ cp => BinOpNode(NumeralNode(-1), VariableNode(x), "*")}
  def varLitteral = variable ^^ {x => VariableNode(x)}
  def negNumber = openParens ~ "-" ~ number ~ closeParens ^^ {case op ~ min ~ x ~ cp => NumeralNode(0 - x.toInt)}
  def value = litteral | varLitteral | negNumber | negVariable
  def term = value | parens
  def parens: Parser[MathNode] = openParens ~> expr <~ closeParens
  def binaryOp(level: Int): Parser[((MathNode, MathNode) => MathNode)] = {
    level match {
      case 1 =>
        "+" ^^^ {(a: MathNode, b: MathNode) => BinOpNode(a, b, "+")}|
        "-" ^^^ {(a: MathNode, b: MathNode) => BinOpNode(a, b, "-")}
      case 2 => 
        "*" ^^^ {(a: MathNode, b: MathNode) => BinOpNode(a, b, "*")}|
        "" ^^^ {(a: MathNode, b: MathNode) => BinOpNode(a, b, "*")}|
        "/" ^^^ {(a: MathNode, b: MathNode) => BinOpNode(a, b, "/")}
      case 3 => 
        "^" ^^^ {(a: MathNode, b: MathNode) => BinOpNode(a, b, "^")}|
        "rt" ^^^ {(a: MathNode, b: MathNode) => BinOpNode(a, b, "rt")}
      case _ => throw new RuntimeException("bad precedence")
    }
  }
  val minPrec = 1
  val maxPrec = 3
  def binary(level: Int): Parser[MathNode] = 
    if (level > maxPrec) term
    else binary(level+1) * binaryOp(level)
  def expr = binary(minPrec) | term
  def apply(input: String): MathNode = parse(expr, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => {println(failure.msg); NumeralNode(-1)}
  }
}

