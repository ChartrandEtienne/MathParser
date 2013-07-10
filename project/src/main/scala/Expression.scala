package parsing

class Expression(val expr: Tree[MathNode]) {
  override def toString = expr.toString
  def getVariables: List[VariableNode] = {
    def rec(tree: Tree[MathNode]): List[VariableNode] = tree match {
      case Node(x: VariableNode, _, _) => x :: Nil
      case Node(a: BinOpNode, x, y) => rec(x) ::: rec(y)
      case _ => Nil
    }
    rec(expr)
  }
  def isPowerSeries: Boolean = {
    def rec(tree: Tree[MathNode]): Boolean = tree match {
      case Node(x: VariableNode, _, _) => true
      case Node(x: NumeralNode, _, _) => true
      case Node(BinOpNode(_, _, "*"), Node(x: NumeralNode, _, _), Node(y: VariableNode, _, _)) => true
      case Node(BinOpNode(_, _, "^"), Node(x: VariableNode, _, _), Node(y: NumeralNode, _, _)) => true
      case Node(BinOpNode(_, _, "*"), Node(x: NumeralNode, _, _), Node(BinOpNode(_, _, "^"), Node(a: VariableNode, _, _), Node(b: NumeralNode, _, _))) => true
      case NNode(BinOpNode(_, _, "+"), x: LNode[Tree[MathNode]]) => x.lfoldr(true, {(y: Tree[MathNode], x: Boolean) => x & rec(y)})
      case Node(BinOpNode(_, _, "+"), x @ Node(a: MathNode, _, _), y @ Node(b: MathNode, _, _)) => {
        println("wot")
        rec(y) & rec(x)
      }      
      case _ => false
    }
    rec(expr)
  }
  def negate = {
    def rec(tree: Tree[MathNode]): Tree[MathNode] = tree match {
      case Nill => tree
//      case Node(bop @ BinOpNode(_, _, "*"), Node(x: MathNode, _, _), Node(y: MathNode, _, _)) => {
      case Node(bop @ BinOpNode(_, _, "*"), Node(NumeralNode(x: Int), _, _), Node(y: MathNode, _, _)) => {
        println("mult")
        Node(bop, Node(NumeralNode(0 - x)), Node(y))
      }
//      case Node(bop @ BinOpNode("*"), Node(NumeralNode(numeral: Int), _, _), right) => new Node(bop, Node(NumeralNode(0 - numeral)), right)
      case node @ Node(value, left, right) => value match {
        case NumeralNode(x) => new Node(NumeralNode(0 - x))
        case x: MathNode => Node(new BinOpNode("*"), Node(NumeralNode(-1)), node)
      }
      case x: NNode[MathNode] => Node(new BinOpNode("*"), Node(NumeralNode(-1)), x)
    }
    new Expression(rec(expr))
  }
  def toFlatten = {
    def rec(tree: Tree[MathNode]): Tree[MathNode] = {
      tree match {
      case Node(bop, left, right) => {
        val recleft = rec(left)
        val recright = rec(right)
        (recleft, recright) match {
          case (left @ Node(op2, left2, right2), right) if (bop.toFlatten(op2)._1) => {
            // println("trouve simple")
            val negs = bop.toFlatten(op2)._2
            val nright2 = if (negs._2) new Expression(rec(right2)).negate.expr else rec(right2)
            val nrecright = if (negs._1) new Expression(recright).negate.expr else recright
//            NNode(bop, LNode(rec(left2), rec(right2), recright))
            NNode(if (negs._1|negs._2) new BinOpNode("+") else bop, LNode(rec(left2), nright2, nrecright))
          }
          case (left, right @ Node(op2, left2, right2)) if (bop.toFlatten(op2)._1) => {
            val negs = bop.toFlatten(op2)._2
            val nrecleft2 = if (negs._1) new Expression(rec(left2)).negate.expr else rec(left2)
            val nrecright2 = if (negs._2 ^ negs._1) new Expression(rec(right2)).negate.expr else rec(right2)
            NNode(if (negs._1|negs._2) new BinOpNode("+") else bop, LNode(recleft, nrecleft2, nrecright2))
          }
          case (left @ NNode(op2, cons), right) if (bop.toFlatten(op2)._1) => {
            // println("trouve complexe")
            // NNode(bop, LNode(recright, cons))
            NNode(bop, cons.append(LNode(recright)))
          }
          case (left, right @ NNode(op2, cons)) if (bop.toFlatten(op2)._1) => {
            // println("trouve complexe")
            // NNode(bop, LNode(recright, cons))
            NNode(bop, cons.append(LNode(recleft)))
          }
          case _ => Node(bop, recleft, recright)
          }
        }
      case _ => tree
    }}
    new Expression(rec(expr))
  }
}
