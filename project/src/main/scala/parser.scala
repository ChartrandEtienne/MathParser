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



abstract class MathNode {
  def eval(): Double
  def eval(valeurVariables: Map[String, Double]): Double
//  def toString2: String
//  def toString3: String
  def toQueue(level: Int, shift: Int): List[(MathNode, Int, Int)]
//  def toTree: Tree[MathNode]
  def toTree: Tree[MathNode]
  def toFlatten(x: MathNode): Boolean
}

case class NullNode extends MathNode {
  def eval() = 0.0
  def eval(valeurVariables: Map[String, Double]) = 0.0
  override def toString = ""
//  def toString2 = ""
//  def toString3 = "" 
  def toQueue(level: Int, shift: Int) = Nil
  def toTree = Nill[MathNode]
  def toFlatten(x: MathNode) = false
}


case class VariableNode(value: String) extends MathNode {
  def eval(): Double = eval(Map())
  override def toString = value.toString
//  def toString2 = value.toString
//  def toString3 = value.toString
  def toQueue(level: Int, shift: Int) = (this, level, shift) :: Nil
  def toTree = new Node(this)
  def eval(valeurVariables: Map[String, Double]) = 
    try {
      valeurVariables(value)
    } catch {
      case x: java.util.NoSuchElementException => throw new RuntimeException("valeur manquante: " + value)
    }
  def toFlatten(x: MathNode) = false
}


case class NumeralNode(value: Int) extends MathNode {
  override def toString = value.toString
//  def toString2 = value.toString
//  def toString3 = value.toString
  def toQueue(level: Int, shift: Int) = (this, level, shift) :: Nil
  def toTree = new Node(this)
  def eval(): Double = eval(Map())
  def eval(valeurVariables: Map[String, Double]) = value.toDouble
  def +(that: NumeralNode) = NumeralNode(that.value + this.value)
  def -(that: NumeralNode) = NumeralNode(that.value - this.value)
  def toFlatten(x: MathNode) = false
}

case class BinOpNode(left: MathNode, right: MathNode, op: String) extends MathNode {
  override def toString = op
//  def toString2 = 
//    "bop(" + op + " " + left.toString2 + " " + right.toString2 + ")"
//  def toString3 = "(  " + op + "  )"
  def toQueue(level: Int, shift: Int) = (this, level, shift) :: left.toQueue(level + 1, shift - 1) ::: right.toQueue(level + 1, shift + 1)
  def toTree = Node[MathNode](BinOpNode(null, null, op), left.toTree, right.toTree)
//  def toTree = left match {
//    case BinOpNode(_, _, lop) if (op == lop) => 
//      nNode[MathNode](BinOpNode(null, null, op), List(left.toTree, right.toTree))
//    case _ => Node[MathNode](BinOpNode(null, null, op), left.toTree, right.toTree)
//  }

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
  }
  def eval(): Double = eval(Map())
  def toFlatten(x: MathNode) = x match {
//    case BinOpNode(_, _, lop) => ((lop == "*") && (op == "*")) || ((lop == "+") && (op == "+")) || ((lop == "-") && (op == "-"))
    case BinOpNode(_, _, lop) => ((op == "+") && (lop == "+"))
    case _ => false 
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

