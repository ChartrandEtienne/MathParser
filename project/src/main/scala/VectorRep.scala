package parsing

class ExprVec(val expr: List[Number]) {

  def +(that: ExprVec) = new ExprVec(zip1(expr, that.expr, _ + _))  

//  def plus(a: List[Number], b: List[Number]) = zip1(a, b, _ + _)

  def zip1(a: List[Number], b: List[Number], f: (Number, Number) => Number): List[Number] = (a, b) match {
    case (x::xs, z::zs) => f(x, z) :: zip1(xs, zs, f)
    case (x::xs, Nil) => x :: zip1(xs, Nil, f)
    case (Nil, x::xs) => x :: zip1(Nil, xs, f)
    case (Nil, Nil) => Nil
  }

  def times(a: List[Number], b: List[Number]): List[Number] = {
    def add(x: List[Number], y: List[Number]) = zip1(x, y, _ + _)
    def rec(x: List[Number], deg: Int): List[List[Number]] = x match {
      case Nil => Nil
      case x :: xs => (List.fill(deg)(Real(0)) ::: b.map(_ * x)) :: rec(xs, deg + 1)
    }
    rec(a, 0).foldLeft(List[Number]())({(x: List[Number], y: List[Number]) => add(x, y)})
  }

  def *(argument: ExprVec): ExprVec = new ExprVec(times(expr, argument.expr))

/*
  def gcd(a: List[Number], b: List[Number]): List[Number] = (a, b) match {
    case (a: List[Number], b: Nil) => List(Number(0))
    case (a: List[Number], b: List[Number]) => gcd(b, (
  }
*/

  def div(a: List[Number], v: List[Number]): (List[Number], List[Number]) = {
    def rec(q: List[Number], r: List[Number]): (List[Number], List[Number]) = {
//      println("wat")
      val coeff = r.last / v.last
      val deg = r.length - v.length
      val mult = poke(List.fill(deg + 1)(Real(0)), deg, coeff)
      val q1 = zip1(q, mult, _ + _).reverse.dropWhile(_ == Real(0)).reverse
      val r1 = zip1(r, times(mult, v), _ - _).reverse.dropWhile(_ == Real(0)).reverse
/*
      println("coeff: " + coeff)
      println("deg: " + deg)
      println("mult: " + mult)
      println("q: " + q.toString)
      println("r: " + r.toString)
      println("q1: " + q1.toString)
      println("r1: " + r1.toString)
*/
//      println("X: " + X.toString)
      if (r1.length < v.length) (q1, r1) else rec(q1, r1)
    }
    if (a.length <= v.length) (List(Real(0)), a) else {
      val res = rec(List.fill(0)(Real(0)), a)
      (res._1, res._2)
    }
  }

  def /(argument: ExprVec): (ExprVec, ExprVec) = {
    val res = div(expr, argument.expr)
    (new ExprVec(res._1), new ExprVec(res._2))
  }
 
  def gcdInternal(a: List[Number], b: List[Number]): List[Number] = {
    def rec(x: List[Number], y: List[Number]): List[Number] = {
      println("x: " + x.toString + " y: " + y.toString)
      (x, y) match {
      case (x: List[Number], Nil) => x
      case (x: List[Number], y: List[Number]) => rec(y, div(x, y)._2)
    }}
    if (a.length < b.length) rec(b, a) else rec(a, b)
  } 

  def gcd(that: ExprVec) = new ExprVec(gcdInternal(expr, that.expr))

  def derivInternal(a: List[Number]): List[Number] = {
    def rec(x: List[Number], deg: Int): List[Number] = (deg, x) match {
      case (0, x :: Nil) => Nil
      case (0, x :: xs) => rec(xs, 1)
      case (c: Int, x :: xs) => x * Real(deg) :: rec(xs, deg + 1)
      case (x: Int , Nil) => Nil
    }
    rec(a, 0)
  }

  def deriv = new ExprVec(derivInternal(expr))

// eh, en fait, il manque le algebraic-expand ou whatever.
// enfin bonne nuit. 
/*
  def factoInternal(expr: List[Number]): List[List[Number]] = {
    val u = factoInternal(expr.map(_ / expr.last))
    val p = List(Number(1))
    val r = u / u.deriv
    val f = u / r
    val j = 1
    def rec(g1: List[Number], s1: List[Number], p1: List[List[Number]], r1: List[Number], r1: List[Number], j: Int) = {
      if (r == List(Number(1)) 
    }
  }
*/

  
  def poke(x: List[Number], i: Int, value: Number) = x.take(i) ::: x(i) + value :: x.drop(i + 1)

/*
    def deg(list: List[Number]) = {
      def neg(x: List[Number], y: Int): Int = x match { case Nil => 0 case x::xs if (x != Number(0)) => y case x :: xs => neg(xs, y - 1) }
      neg(list, list.length)
    }
*/

/*
  def /(that: ExprVec) = {
    def rec(q: List[Number], r: List[Number]): (List[Number], List[Number]) = {
      if (r.length < v.length) (q, r)
      else {
        val q1 = 
      }
    }
    new ExprVec(rec(Nil, expr))
  }
*/

  override def toString = expr.toString
}

