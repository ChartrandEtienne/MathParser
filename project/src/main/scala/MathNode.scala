package parsing
import math.{pow, sqrt, cbrt}

abstract class MathNode {
  def eval(): Double
  def eval(valeurVariables: Map[String, Double]): Double
  def toFlatten(x: MathNode): (Boolean, (Boolean, Boolean))
}

case object NullNode extends MathNode {


  def eval() = 0.0
  def eval(valeurVariables: Map[String, Double]) = 0.0
  override def toString = ""
  def toFlatten(x: MathNode) = (false, (false, false))
}


case class VariableNode(value: String) extends MathNode {
  def eval(): Double = eval(Map())
  override def toString = value.toString
  def eval(valeurVariables: Map[String, Double]) = 
    try {
      valeurVariables(value)
    } catch {
      case x: java.util.NoSuchElementException => throw new RuntimeException("valeur manquante: " + value)
    }
  def toFlatten(x: MathNode) = (false, (false, false))
}


case class NumeralNode(value: Int) extends MathNode {
  override def toString = value.toString
  def eval(): Double = eval(Map())
  def eval(valeurVariables: Map[String, Double]) = value.toDouble
  def +(that: NumeralNode) = NumeralNode(that.value + this.value)
  def -(that: NumeralNode) = NumeralNode(that.value - this.value)
  def toFlatten(x: MathNode) = (false, (false, false))
  def negate = NumeralNode(0 - value)
}


case class BinOpNode(left: MathNode, right: MathNode, op: String) extends MathNode {
//  override def toString = op
//  def toString2 = 
//    "bop(" + op + " " + left.toString2 + " " + right.toString2 + ")"
  override def toString = "(" + left.toString + " " + op + " " + right.toString + ")"

  def apply(leftargnode: NumeralNode, rightargnode: NumeralNode): NumeralNode = {
    val (NumeralNode(leftarg), NumeralNode(rightarg)) = (leftargnode, rightargnode)
    NumeralNode(op match {
      case "+" => leftarg + rightarg
      case "-" => leftarg - rightarg
      case "*" => leftarg * rightarg
      case "/" => leftarg / rightarg
      case "^" => pow(leftarg, rightarg).toInt
      case "rt" => leftarg match {
        case 1 => rightarg
        case 2 => sqrt(rightarg).toInt
        case 3 => cbrt(rightarg).toInt // enfin bon
      }
    })
  }

  def this(value: String) = this(NullNode, NullNode, value)

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
    
    case BinOpNode(_, _, lop) if ((op == "+")|(op == "-")) & ((lop == "+")|(lop == "-")) => (true, ((op == "-"), (lop == "-")))
//    case BinOpNode(_, _, lop) if ((op == "+") && (lop == "+")) => (true, (false, false))
//    case BinOpNode(_, _, lop) if ((op == "-") && (lop == "+")) => (true, (false, true))
//    case BinOpNode(_, _, lop) if ((op == "+") && (lop == "-")) => (true, (true, false))
//    case BinOpNode(_, _, lop) if ((op == "-") && (lop == "-")) => (true, (true, true))
    case BinOpNode(_, _, lop) if ((op == "*") && (lop == "*")) => (true, (false, false))
    case _ => (false, (false, false))
  }
}
