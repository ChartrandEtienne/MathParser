package parsing

class Expression(val expr: Tree[MathNode]) {
  override def toString = expr.toString
  def negate = {
    def rec(tree: Tree[MathNode]): Tree[MathNode] = tree match {
      case Nill => tree
      case node @ Node(value, left, right) => value match {
        case NumeralNode(x) => new Node(NumeralNode(0 - x))
        case x: MathNode => Node(new BinOpNode("*"), new Node(NumeralNode(-1)), node)
      }
      case x: NNode[MathNode] => new Node(new BinOpNode("*"), new Node(NumeralNode(-1)), x)
    }
    new Expression(rec(expr))
  }
  def toPowerSeries = {
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
            NNode(if (negs._1|negs._2) new BinOpNode(NullNode, NullNode, "+") else bop, LNode(rec(left2), nright2, nrecright))
          }
          case (left @ NNode(op2, cons), right) if (bop.toFlatten(op2)._1) => {
            // println("trouve complexe")
            // NNode(bop, LNode(recright, cons))
            NNode(bop, cons.append(LNode(recright)))
          }
          case _ => Node(bop, recleft, recright)
        }
      }
      case _ => tree
    }}
    new Expression(rec(expr))
  }
}
