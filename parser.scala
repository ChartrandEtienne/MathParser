import scala.util.matching.Regex
import java.util.regex.Pattern
import scala.util.parsing.combinator.RegexParsers
import math.{pow, sqrt, cbrt}

/* algorithmes:
 * separer les monomes des polynome
 * - algo de multiplication "brute" entre polynomes
 * - algo de collection des termes similaires dans un polynome
*/

class MathLexer extends RegexParsers {
  def posNumber: Parser[String] = regex(new Regex("[0-9]+")) // " 
  def negNumber: Parser[String] = regex(new Regex("-[0-9]+")) // " 
  def number = posNumber | negNumber
  def variable: Parser[String] = regex(new Regex("[a-z]")) // "
  def operator: Parser[String] = regex(new Regex("[/*/^//+-]")) | "rt"  //"
  def exprLex = rep(number | variable | operator)
//  def openParens: Parser[String] = regex(new Regex("[/(]"))  //" 
//  def closeParens: Parser[String] = regex(new Regex("[/)]"))  // "
  def openParens: Parser[String] = "("
  def closeParens: Parser[String] = ")"
  def applyLex(input: String): List[String] = parse(exprLex, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => {println(failure.msg); Nil}
  }
}

abstract class MathNode {
  def eval(): Double
  def eval(valeurVariables: Map[String, Double]): Double

}

case class VariableNode(value: String) extends MathNode {
  def eval(): Double = eval(Map())
  override def toString = value
  def eval(valeurVariables: Map[String, Double]) = 
    try {
      valeurVariables(value)
    } catch {
      case x: java.util.NoSuchElementException => throw new RuntimeException("valeur manquante: " + value)
    }
}


case class NumeralNode(value: Int) extends MathNode {
  override def toString = value.toString
  def eval(): Double = eval(Map())
  def eval(valeurVariables: Map[String, Double]) = value.toDouble
  def +(that: NumeralNode) = NumeralNode(that.value + this.value)
  def -(that: NumeralNode) = NumeralNode(that.value - this.value)
}

case class BinOpNode(left: MathNode, right: MathNode, op: String) extends MathNode {
  override def toString = 
    if(op == "+" | op == "-" | op == "/") left.toString + " " + op + " " + right.toString
    else if (op == "^") "(" + left.toString + op + right.toString + ")"
    else if (op == "*") "(" + left.toString + right.toString + ")"
    else if (op == "rt") "\\" + left.toString + "/" + right.toString + "\\"
    else "can't toString that"
  def eval(valeurVariables: Map[String, Double]) = { 
    val leftEval = left.eval(valeurVariables)
    val rightEval = right.eval(valeurVariables)
    op match {
      case "+" => leftEval + rightEval
      case "-" => leftEval - rightEval
      case "*" => leftEval * rightEval
      case "/" => leftEval / rightEval
      case "^" => pow(leftEval, rightEval)
      case "rt" => leftEval match {
        case 1 => rightEval
        case 2 => sqrt(rightEval)
        case 3 => cbrt(rightEval)
      }
    } 
/*
  op match {
    case "+" => left.eval(valeurVariables) + right.eval(valeurVariables)
    case "-" => left.eval(valeurVariables) - right.eval(valeurVariables)
    case "*" => left.eval(valeurVariables) * right.eval(valeurVariables)
    case "/" => left.eval(valeurVariables) / right.eval(valeurVariables)
    case "^" => pow(left.eval(valeurVariables), right.eval(valeurVariables))
    case "rt" => left.eval(valeurVariables) match  {
      case 1 => right.eval(valeurVariables)
      case 2 => sqrt(right.eval(valeurVariables))
      case 3 => cbrt(right.eval(valeurVariables))
      case _ => 0
    }
  }
*/
  }
  def eval(): Double = eval(Map())
}


object p extends MathLexer {
  def litteral = number ^^ {x => NumeralNode(x.toInt)}

  def varLitteral = variable ^^ {x => VariableNode(x)}

  def value = litteral | varLitteral

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


