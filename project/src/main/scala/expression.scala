package parsing

class Expression(val expr: Tree[MathNode]) {
  override def toString = expr.toString
  def toPowerSeries = {
    def rec(tree: Tree[MathNode]): Tree[MathNode] = {
      println("rec")
      tree match {
      case Node(bop, left, right) => {
        val recleft = rec(left)
        val recright = rec(right)
        (recleft, recright) match {
          case (left @ Node(op2, left2, right2), right) if (bop.toFlatten(op2)) => {
            println("trouve simple")
            NNode(bop, LNode(rec(left2), rec(right2), recright))
          }
          case (left @ NNode(op2, cons), right) if (bop.toFlatten(op2)) => {
            println("trouve complexe")
            NNode(bop, LNode(recright, cons))
          }
          case _ => Node(bop, recleft, recright)
        }
      }
      case _ => tree
    }}
    rec(expr)
  }
}
