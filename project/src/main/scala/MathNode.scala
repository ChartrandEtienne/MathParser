package parsing
import math.{pow, sqrt, cbrt}

trait MathNode {
  def eval(): Number 
  def eval(valeurVariables: Map[String, Number]): Number 
  def toFlatten(x: MathNode): (Boolean, (Boolean, Boolean))
}

case class VariableNode(val value: String) extends MathNode {
  override def toString = value.toString
  override def equals(o: Any) = o match {
    case VariableNode(x) if (x == value) => true
    case _ => false
  }
  def eval(): Number = eval(Map())
  def eval(valeurVariables: Map[String, Number]) = 
    try {
      valeurVariables(value)
    } catch {
      case x: java.util.NoSuchElementException => throw new RuntimeException("valeur manquante: " + value)
    }
  def toFlatten(x: MathNode) = (false, (false, false))
}



case class NumeralNode(val value: Number) extends MathNode {
  override def toString = value.toString
  override def equals(o: Any) = o match {
    case NumeralNode(x) if (x == value) => true
    case _ => false
  }
  def eval(): Number = eval(Map())
  def eval(valeurVariables: Map[String, Number]) = value
  def +(that: NumeralNode) = NumeralNode(that.value + this.value)
  def -(that: NumeralNode) = NumeralNode(that.value - this.value)
  def toFlatten(x: MathNode) = (false, (false, false))
  def negate = NumeralNode(Real(0) - value)
  def invert = value match {
    case Real(x, y) => NumeralNode(Real(y, x))
    case Float(x) => NumeralNode(Float(1 / x))
  }
}

class AssOpNode(val op: String, val args: List[MathNode]) extends MathNode {
  override def toString = "[" + op + " " + args.mkString(", ") + "]"

  def eval(valeurVariables: Map[String, Number]) = {
    val argsEval = args.map(_.eval(valeurVariables))
    op match {
      case "+" => argsEval.reduceLeft(_ + _)
      case "*" => argsEval.reduceLeft(_ * _)
      case _ => {
        throw new RuntimeException("operateur incorrect: " + op)
        Real(0)
      }
    }
  } 

  def toFlatten(x: MathNode) = x match {
    
    case BinOpNode(_, _, lop) if ((op == "+")|(op == "-")) & ((lop == "+")|(lop == "-")) => (true, ((op == "-"), (lop == "-")))
    case BinOpNode(_, _, lop) if ((op == "*") && (lop == "*")) => (true, (false, false))
    case _ => (false, (false, false))
  }

  def eval(): Number = eval(Map())

}

object AssOpNode {
  def apply(op: String, args: List[MathNode]) = args match {
    case x :: y :: Nil => new BinOpNode(op, x, y)
    case x: List[MathNode] => new AssOpNode(op, x)
  }
  def unapply(x: MathNode) = x match {
    case x: AssOpNode => Some((x.op, x.args))
    case x: BinOpNode => Some((x.op, x.left :: x.right :: Nil))
    case _ => None
  }
}

object BinOpNode {
  def apply(op: String, left: MathNode, right: MathNode) = new BinOpNode(op, left, right)
  def unapply(x: BinOpNode) = Some((x.op, x.left, x.right))
}

class BinOpNode(val op: String, val left: MathNode, val right: MathNode) extends MathNode {
//  override def toString = op
//  def toString2 = 
//    "bop(" + op + " " + left.toString2 + " " + right.toString2 + ")"
//  override def toString = "(" + left.toString + " " + op + " " + right.toString + ")"
    override def toString = "(" + op + ", " + left.toString + ", " + right.toString + ")"

  def eval(valeurVariables: Map[String, Number]) = { 
    val leftEval = left.eval(valeurVariables)
    val rightEval = right.eval(valeurVariables)
    op match {
      case "+" => leftEval + rightEval
      case "-" => leftEval - rightEval
      case "*" => leftEval * rightEval
      case "/" => leftEval / rightEval
      
      case "^" => Real(43)
//      case "^" => pow(leftEval, rightEval)
      case "rt" => Real(42)
/*
      case "rt" => leftEval match {
        case 1 => rightEval
        case 2 => sqrt(rightEval)
        case 3 => cbrt(rightEval)
      }
*/
    } 
  }
  def eval(): Number = eval(Map())
  def toFlatten(x: MathNode) = x match {
    
    case BinOpNode(_, _, lop) if ((op == "+")|(op == "-")) & ((lop == "+")|(lop == "-")) => (true, ((op == "-"), (lop == "-")))
    case BinOpNode(_, _, lop) if ((op == "*") && (lop == "*")) => (true, (false, false))
    case _ => (false, (false, false))
  }
}

