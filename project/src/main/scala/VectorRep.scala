package parsing

class Real(val num: Int, val denom: Int) {
  override def toString = if (denom == 1) num.toString else num + "/" + denom
  def create(x: Int, y: Int): (Int, Int) = {
    def gcd(a: Int, b: Int): Int = if (a % b == 0) b else gcd(b, a % b)
    val z = gcd(x, y)
    (x / z, y / z)
  }
  override def equals(o: Any): Boolean = o match {
    case that: Real if ((that.num == num) && (that.denom == denom)) => true
    case that: Real if ((that.num == 0) && (num == 0)) => true
    case that: Int if ((that == num) && (denom == 1)) => true
    case that: Int if ((that == 0) && (num == 0)) => true
    case _ => false
  }
  def +(that: Real): Real = {
    val thisNum2 = num * that.denom
    val denom2 = denom * that.denom
    val thatNum2 = that.num * denom
    Real(thisNum2 + thatNum2, denom2)
  }
  def -(that: Real): Real = {
    val thisNum2 = num * that.denom
    val denom2 = denom * that.denom
    val thatNum2 = that.num * denom
    Real(thisNum2 - thatNum2, denom2)
  }
  def *(that: Real): Real = {
    val num2 = num * that.num
    val denom2 = denom * that.denom
    val z = create(num2, denom2)
    Real(z._1, z._2)
  }
  def /(that: Real): Real = {
    val num2 = num * that.denom
    val denom2 = denom * that.num
    Real(num2, denom2)
  }
  def toInt = num / denom
}

object Real {
  
  def create(x: Int, y: Int): (Int, Int) = {
    def gcd(a: Int, b: Int): Int = if (a % b == 0) b else gcd(b, a % b)
    val z = gcd(x, y).abs
    if (x == 0) (0, 1) else 
    if ((x < 0) && (y < 0)) ((x / z).abs, (y / z).abs)else
    if ((x > 0) && (y < 0)) (-(x / z), -(y / z)) else 
    (x / z, y / z)
  }
  def apply(x: Int) = new Real(x, 1)
  def apply(x: Int, y: Int) = new Real(create(x, y)._1, create(x, y)._2)
}

class ExprVec(val expr: List[Real]) {

  def +(that: ExprVec) = new ExprVec(zip1(expr, that.expr, _ + _))  

//  def plus(a: List[Real], b: List[Real]) = zip1(a, b, _ + _)

  def zip1(a: List[Real], b: List[Real], f: (Real, Real) => Real): List[Real] = (a, b) match {
    case (x::xs, z::zs) => f(x, z) :: zip1(xs, zs, f)
    case (x::xs, Nil) => x :: zip1(xs, Nil, f)
    case (Nil, x::xs) => x :: zip1(Nil, xs, f)
    case (Nil, Nil) => Nil
  }

  def times(a: List[Real], b: List[Real]): List[Real] = {
    def add(x: List[Real], y: List[Real]) = zip1(x, y, _ + _)
    def rec(x: List[Real], deg: Int): List[List[Real]] = x match {
      case Nil => Nil
      case x :: xs => (List.fill(deg)(Real(0)) ::: b.map(_ * x)) :: rec(xs, deg + 1)
    }
    rec(a, 0).foldLeft(List[Real]())({(x: List[Real], y: List[Real]) => add(x, y)})
  }

  def *(argument: ExprVec): ExprVec = new ExprVec(times(expr, argument.expr))
/*
    val b = argument.expr
    def add(x: List[Real], y: List[Real]) = zip1(x, y, _ + _)
    def rec(x: List[Real], deg: Int): List[List[Real]] = x match {
      case Nil => Nil
      case x :: xs => (List.fill(deg)(Real(0)) ::: b.map(_ * x)) :: rec(xs, deg + 1)
    }
    new ExprVec(rec(expr, 0).foldLeft(List[Real]())({(x: List[Real], y: List[Real]) => add(x, y)}))
  }
*/

  def /(argument: ExprVec): (ExprVec, ExprVec) = {
    val v = argument.expr

    def rec(q: List[Real], r: List[Real]): (List[Real], List[Real]) = {
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
    if (expr.length <= argument.expr.length) (new ExprVec(List(Real(0))), new ExprVec(expr)) else {
      val res = rec(List.fill(0)(Real(0)), expr)
      (new ExprVec(res._1), new ExprVec(res._2))
    }
  }
  
  def poke(x: List[Real], i: Int, value: Real) = x.take(i) ::: x(i) + value :: x.drop(i + 1)

/*
    def deg(list: List[Real]) = {
      def neg(x: List[Real], y: Int): Int = x match { case Nil => 0 case x::xs if (x != Real(0)) => y case x :: xs => neg(xs, y - 1) }
      neg(list, list.length)
    }
*/

/*
  def /(that: ExprVec) = {
    def rec(q: List[Real], r: List[Real]): (List[Real], List[Real]) = {
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

