package parsing

class Expression(val expr: Tree[MathNode]) {
  override def toString = expr.toString
  def eval: Expression = eval(Map())
  def eval(valeurVariables: Map[String, Int]): Expression = {
    def rec(tree: Tree[MathNode]): Tree[MathNode] = tree match {
      case a @ Node(x: NumeralNode, _, _) => a
      case a @ Node(VariableNode(x), _, _) => valeurVariables.get(x) match {
        case Some(numeral) => new Node(NumeralNode(numeral))
        case None => a
      }
      case Node(bop: BinOpNode, left, right) => {
        (rec(left), rec(right)) match {
          case (Node(leftnum: NumeralNode, _, _), Node(rightnum: NumeralNode, _, _)) => {
            println("Node")
            println(leftnum + ", " + rightnum)
            new Node(bop.apply(leftnum, rightnum))
          }
          case (x, y) => {
            println("wot")
            println(x + ", " + y)
            Node(bop, x, y)
          }
          case _ => {
            println("nope")
            Node(bop, rec(left), rec(right))
          }
        }
      }
      case NNode(bop: BinOpNode, cons: LNode[Tree[MathNode]]) => {
        println("NNode1")
        val result = cons.lmap(rec).lfoldr(List[Tree[MathNode]](), {(value: Tree[MathNode], seed: List[Tree[MathNode]]) => (value, seed) match {
          case (x @ Node(number2: NumeralNode, _, _), num @ Node(number: NumeralNode, _, _) :: y) => {
            println("number3: " + x + " || " + number + " || " + y)
            new Node(bop.apply(number2, number2)) :: y
          }
          case (x, num @ Node(number: NumeralNode, _, _) :: y) => {
            println("number2: " + x + " || " + number + " || " + y)
            Node(number) :: x :: y
          }
          case (Node(number: NumeralNode, _, _), y) => {
            println("number: " + number + " || " + y)
            Node(number) :: y
          }
          case (x, y) => {
            println("wut: " + x + " || " + y)
            x :: y
          }
          
        }})
        new NNode(bop, LNode(result.head, result.tail))
      }
/*
      case NNode(bop: BinOpNode, cons: LNode[Tree[MathNode]]) => {
        println("NNode1")
        cons.lreducer({(accu: Tree[MathNode], iter: Tree[MathNode]) => (rec(iter), rec(accu)) match {
          case (x @ Node(wat: NumeralNode, _, _), y @ Node(wut: NumeralNode, _, _)) => {
            println("NNode")
            println(x + ", " + y)
            Node(bop.apply(wat, wut))
          }
          case (x @ Node(wat: BinOpNode, _, _), y @ Node(wut: NumeralNode, _, _)) => {
            println("wot6")
            println(x + " || " + y)
            println(wat == bop)
//            println(wit)
            Node(bop, y, x)
          }
          case (x, y) => {
            println("wot4")
            println(x + ", " + y)
            Node(bop, x, y)
          }
// rrrrrhhhhaaaaahhhhhh please kill me
        }})
      }
*/
      case _ => tree
    }
//    new Expression(rec(expr)).toFlatten
    new Expression(rec(expr))
  }
  def getVariables: List[VariableNode] = {
    def rec(tree: Tree[MathNode]): List[VariableNode] = tree match {
      case Node(x: VariableNode, _, _) => x :: Nil
      case Node(a: BinOpNode, x, y) => rec(x) ::: rec(y)
//      case NNode(a: BinOpNode, cons) => cons.lfoldr(Nil, {(x: Tree[MathNode], y: List[VariableNode]) => rec(x) :: y})
      case NNode(a: BinOpNode, cons) => cons.lfoldr(Nil, {(x: Tree[MathNode], y: List[VariableNode]) => rec(x) ::: y})
      case _ => Nil
    }
    rec(expr).distinct
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
