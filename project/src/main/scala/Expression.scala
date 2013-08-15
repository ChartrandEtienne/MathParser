package parsing

class Expression(val expr: Tree[MathNode]) {
  override def toString = expr.toString

  def toVectorRep = new ExprVec(toVector)

  def toVector: List[Real] = {
    def poke(x: List[Int], i: Int, value: Int) = x.take(i) ::: x(i) + value :: x.drop(i + 1)
    def degQuot(x: Tree[MathNode]) = x match {
      case Node(NumeralNode(x: Int), _, _) => (0, x)
      case Node(x: VariableNode, _, _) => (1, 1)
      case Node(BinOpNode(_, _, "*"), Node(NumeralNode(y: Int), _, _), Node(x: VariableNode, _, _)) => (1, y)
      case Node(BinOpNode(_, _, "*"), Node(NumeralNode(y: Int), _, _), Node(BinOpNode(_, _, "^"), Node(x: VariableNode, _, _), Node(NumeralNode(deg), _, _))) => (deg, y)
      case Node(BinOpNode(_, _, "^"), Node(x: VariableNode, _, _), Node(NumeralNode(deg), _, _)) => (deg, 1)
      case x => {
        println("wat:" + x.toString)
        (-7, -5)
      }
    }
    

    this.degPowerSeries match {
    case x: Int if (x >= 0) => {
      expr match {
        case Node(BinOpNode(_, _, "+"), left, right) => {
          val leftDeg = degQuot(left)
          val rightDeg = degQuot(right)
          val degrees2 = poke(List.fill(x+1)(0), leftDeg._1, leftDeg._2)
          poke(degrees2, rightDeg._1, rightDeg._2).map(Real(_))
        }
        case NNode(BinOpNode(_, _, "+"), cons) => {
          val degs = cons.lmap({x: Tree[MathNode] => degQuot(x)})
          degs.lfoldr(List.fill(x+1)(0), {(x: (Int, Int), accu: List[Int]) => {
            poke(accu, x._1, x._2)
          }}).map(Real(_))
        }
      }
    }
    case x: Int => List.fill(0)(Real(0))
    }
  }

  // this guy will barf or throw an error if it's not passed a proper power serie.
  // or more like just return bullshit because I feel like it. 
  def degPowerSeries: Int = expr match {
    case Node(BinOpNode(_, _, "+"), left, right) => {
      val degLeft = left match {
        case Node(x: NumeralNode, _, _) => 0
        case Node(x: VariableNode, _, _) => 1
        case Node(BinOpNode(_, _, "*"), Node(y: NumeralNode, _, _), Node(x: VariableNode, _, _)) => 1
        case Node(BinOpNode(_, _, "*"), _, Node(BinOpNode(_, _, "^"), _, Node(NumeralNode(deg), _, _))) => deg
        case Node(BinOpNode(_, _, "^"), _, Node(NumeralNode(deg), _, _)) => deg
        case _ => -42
      }
      val degRight = right match {
        case Node(x: NumeralNode, _, _) => 0
        case Node(x: VariableNode, _, _) => 1
        case Node(BinOpNode(_, _, "*"), Node(y: NumeralNode, _, _), Node(x: VariableNode, _, _)) => 1
        case Node(BinOpNode(_, _, "*"), _, Node(BinOpNode(_, _, "^"), _, Node(NumeralNode(deg), _, _))) => deg
        case Node(BinOpNode(_, _, "^"), _, Node(NumeralNode(deg), _, _)) => deg
        case _ => -42
      }
    math.max(degLeft, degRight)
    }
    case NNode(BinOpNode(_, _, "+"), cons) => {
      val degrees = cons.lmap({x => x match {
        case Node(x: NumeralNode, _, _) => 0
        case Node(x: VariableNode, _, _) => 1
        case Node(BinOpNode(_, _, "*"), Node(y: NumeralNode, _, _), Node(x: VariableNode, _, _)) => 1
        case Node(BinOpNode(_, _, "*"), _, Node(BinOpNode(_, _, "^"), _, Node(NumeralNode(deg), _, _))) => deg
        case Node(BinOpNode(_, _, "^"), _, Node(NumeralNode(deg), _, _)) => deg
        case _ => -42
      }})
      degrees.lfoldr(0, {(x: Int, y: Int) => math.max(x, y)})
    }
    case _ => -43
  }

  def simplify = {

    def rec(tree: Tree[MathNode]): Tree[MathNode] = tree match {
      case nnode @ NNode(BinOpNode(_, _, "+"), LNode(foo, bar)) => {
        nnode
      }
      case nnode @ NNode(BinOpNode(_, _, "+"), lnode: LNode[Tree[MathNode]]) => {
        lnode.value
      }
      case _ => tree
    }
    new Expression(rec(expr))
  }

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
            new Node(bop.apply(leftnum, rightnum))
          }
          case (x, y) => {
            Node(bop, x, y)
          }
          case _ => {
            Node(bop, rec(left), rec(right))
          }
        }
      }


      case NNode(bop: BinOpNode, cons: LNode[Tree[MathNode]]) => {
        val result = cons.lmap(rec).lfoldr(List[Tree[MathNode]](), {(value: Tree[MathNode], seed: List[Tree[MathNode]]) => (value, seed) match {
          case (x @ Node(number2: NumeralNode, _, _), num @ Node(number: NumeralNode, _, _) :: y) => {
            new Node(bop.apply(number2, number2)) :: y
          }
          case (x, num @ Node(number: NumeralNode, _, _) :: y) => {
            Node(number) :: x :: y
          }
          case (Node(number: NumeralNode, _, _), y) => {
            Node(number) :: y
          }
          case (x, y) => {
            x :: y
          }
          
        }})
        new NNode(bop, LNode(result.head, result.tail))
      }
      case _ => tree
    }
    def rec2(tree: Tree[MathNode]): Tree[MathNode] = tree match {
      case nnode @ NNode(bop @ BinOpNode(_, _, "+"), LNode(foo, bar)) => {
//        rec2(nnode)
        NNode(bop, LNode(rec2(foo), bar))
      }
      case nnode @ NNode(BinOpNode(_, _, "+"), lnode: LNode[Tree[MathNode]]) => {
        rec2(lnode.value)
      }
      case Node(value, left, right) => Node(value, rec2(left), rec2(right))
      case _ => tree
    }
    new Expression(rec2(rec(expr)))
  }
  def getVariables: List[VariableNode] = {
    def rec(tree: Tree[MathNode]): List[VariableNode] = tree match {
      case Node(x: VariableNode, _, _) => x :: Nil
      case Node(a: BinOpNode, x, y) => rec(x) ::: rec(y)
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
      case Node(bop @ BinOpNode(_, _, "*"), Node(NumeralNode(x: Int), _, _), wat @ Node(y: MathNode, _, _)) => {
        Node(bop, Node(NumeralNode(0 - x)), wat)
      }
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

//      tree match {
      val wot = tree match {
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
            NNode(bop, cons.append(LNode(recright)))
          }
          case (left, right @ NNode(op2, cons)) if (bop.toFlatten(op2)._1) => {
            NNode(bop, cons.append(LNode(recleft)))
          }
          case _ => Node(bop, recleft, recright)
          }
        }
      case _ => tree
    }
    wot
    }
    new Expression(rec(expr))
  }
}

object Expression {
  def fromVector(vec: List[Int], deg: Int, variable: String): List[Tree[MathNode]] = vec match {
    case x::xs if (x == 0) => fromVector(xs, deg + 1, variable)
    case x::xs if (deg == 0) => Node(NumeralNode(x)) :: fromVector(xs, deg + 1, variable)
    case x::xs if (x == 1) => Node(new BinOpNode("^"), Node(VariableNode(variable)), Node(NumeralNode(deg))) :: fromVector(xs, deg + 1, variable)
    case x::xs => Node(new BinOpNode("*"), Node(NumeralNode(x)), Node(new BinOpNode("^"), Node(VariableNode(variable)), Node(NumeralNode(deg)))) :: fromVector(xs, deg + 1, variable)
    case Nil => Nil
  }

  def fromVector2(newVector: List[Tree[MathNode]]): Tree[MathNode] = newVector match {
    case Nil => Node(NumeralNode(-42))
    case x :: Nil => x
    case x :: xs :: Nil => Node(new BinOpNode("+"), x, xs)
//    case x: List[Tree[MathNode]] => LNode(new BinOpNode("+"), new LNode(x))
    case x :: xs => NNode(new BinOpNode("+"), LNode(x))
  }
  


}
