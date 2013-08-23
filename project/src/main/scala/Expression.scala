package parsing

class Expression(val expr: MathNode) {
  implicit def number2Node(x: Number): MathNode = NumeralNode(x)
  override def toString = expr.toString

  def testEquivalent: Boolean = expr match {
    case BinOpNode("+", left, right) => {
      println("trouve")
      equivalent(left, right)
    }
    case _ => false 
  }

  def equivalent(a: MathNode, b: MathNode): Boolean = {
    def set(x: List[MathNode], y: List[MathNode]): Boolean = {
      def rec(m: (List[MathNode], List[MathNode]), n: (List[MathNode], List[MathNode])): Boolean = (m, n) match {
        case ((Nil, Nil), (Nil, Nil)) => true
        case ((x :: xs, Nil), (Nil, Nil)) => false
        case ((Nil, Nil), (x :: xs, Nil)) => false
        case ((y :: ys, Nil), (Nil, x :: xs)) => false
        case ((x :: xs, xx), (y :: ys, yy)) => if (equivalent(x, y)) rec((xs ::: xx, Nil), (ys ::: yy, Nil)) else rec((x :: xs, xx), (ys, y :: yy))
      }
      rec((x, Nil), (y, Nil))
    }
    (a, b) match {
      case (AssOpNode(op1, args1), AssOpNode(op2, args2)) if ((op1 == "+" && op2 == "+") || (op1 == "*" && op2 == "*")) => set(args1, args2)
      case (BinOpNode(op1, l1, r1), BinOpNode(op2, l2, r2)) => (op1 == op2) && equivalent(l1, l2) && equivalent(r1, r2)
      case (x: VariableNode, y: VariableNode) => (x == y)
      case (x: NumeralNode, y: NumeralNode) => (x == y)
      case _ => false
    } 
  }

//  def flat = new Expression(invertOpsInternal(expr))
  def flat = new Expression(flattenInternal(invertOpsInternal(expr)))
    

  // this guy just replaces some operators by their invert, when that makes stuff easier to handle.
  // so far, it'd be best to have 
  // a - b = a + neg(b)
  // a / b = a * inv(b)
  // a rt b = a ^ inv(b)

  // I think it's worth to mention that those guys can only be binary operations, so I finding an n-ary op here is a pretty Bad Thing; a - b - c just should't like exist in the system. 
  def invertOpsInternal(x: MathNode): MathNode = x match {
    case BinOpNode("-", left, right) => BinOpNode("+", invertOpsInternal(left), invertOpsInternal(negateInternal(right)))
    case BinOpNode("/", left, right) => BinOpNode("*", invertOpsInternal(left), invertOpsInternal(invertInternal(right)))
    case BinOpNode("rt", left, right) => BinOpNode("*", invertOpsInternal(left), invertOpsInternal(invertInternal(right)))
    case _ => x
  }

  // assumes that every non-associative operator was weeded out beforhand. 
  def flattenInternal(x: MathNode): MathNode = {
      x match {
      case AssOpNode(op, terms) => { 
        val result = terms.map(flattenInternal(_)).foldLeft((List[MathNode](), List[MathNode]()))({(seed, elem) => elem match {
          case x: NumeralNode => (x :: seed._1, seed._2)
          case x: VariableNode => (x :: seed._1, seed._2)
          case AssOpNode(op1, terms1) if (op1 == op) => (terms1 ::: seed._1, seed._2)
          case x: MathNode => (seed._1, x :: seed._2)
        }})
        AssOpNode(op, result._1 ::: result._2 )
      }
      case x: MathNode => x
    }
  }

  def negateInternal(x: MathNode): MathNode = x match {
    case y: NumeralNode => y.negate
    case AssOpNode("*", ops) => AssOpNode("*", Real(-1) :: ops)
    case BinOpNode("-", left, right) => BinOpNode("+", left, negateInternal(right))
    case y: MathNode => BinOpNode("*", Real(-1), y)
  }

  def negative = new Expression(negateInternal(expr))

  def invertInternal(x: MathNode): MathNode = x match {
    case y: NumeralNode => y.invert
    case AssOpNode("*", ops) => AssOpNode("*", ops.map(invertInternal(_)))
    case y: MathNode => BinOpNode("*", Real(-1), y)
  }

  def invert = new Expression(invertInternal(expr))
}
