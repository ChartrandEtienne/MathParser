package parsing

class LNode[T](val value: T, cons: Option[LNode[T]]) {
  override def toString = "(" + this.lfoldr("", {(x: T, y: String) => x.toString + ", " + y}) + ")"
  def toString2 = "LNode wtf"
  def lmap[A](f: (T) => A): LNode[A] = cons match { case Some(x) => new LNode(f(value), Some(x.lmap(f))) case None => new LNode(f(value), None) }
  def lfoldr[A](accu: A, f: (T, A) => A): A = cons match { case Some(x) => f(value, x.lfoldr(accu, f)) case None => f(value, accu) }
}

object LNode {
  def apply[T](value: T, cons: LNode[T]): LNode[T] = new LNode(value, Some(cons))
  def apply[T](value: T, cons: Seq[T]): LNode[T] = cons.toList match { case Nil => new LNode(value, None) case x :: Nil => new LNode(value, Some(new LNode(x, None))) case x :: xs => new LNode(value, Some(this(x, xs))) }
  def apply[T](value: T*): LNode[T] = this(value.head, value.tail)
}

abstract class Tree[T] {
  def toString2 = "tree wtf"
  def map[A](f: (T) => A): Tree[A]
  def foldFlat[A](accu: A)(fun: (A, T) => A): A
//  def flatten(f: (T, T) => Boolean): Tree[T] 
//  def reduceTree[A](accu: A, fun: (A, T, A) => A): A
//  def fold[A](accu: A, fun: (T, List[A]) => A): A
  def fold[A](accu: A, fun: (T, LNode[A]) => A): A
  def twoFold[A](accu: A, fun: (T, A, A) => A): A
}

case class Nill[T] extends Tree[T] {
  override def toString = ""
  override def toString2 = "Nill wtf"
  def map[A](f: (T) => A) = new Nill[A]
  def foldFlat[A](accu: A)(fun: (A, T) => A) = accu
//  def flatten(f: (T, T) => Boolean) = this
  def reduceTree[A](accu: A, fun: (A, T, A) => A) = accu
//  def fold[A](accu: A, fun: (T, List[A]) => A) = accu
  def fold[A](accu: A, fun: (T, LNode[A]) => A) = accu
  def twoFold[A](accu: A, fun: (T, A, A) => A) = accu
}

case class NNode[T](val value: T, cons: LNode[Tree[T]]) extends Tree[T] {
  override def toString = "NNode( " + value.toString + "): " + cons.toString
  override def toString2 = "NNode wtf"
  def map[A](f: (T) => A) = NNode(f(value), cons.lmap(_.map(f)))
  def foldFlat[A](accu: A)(fun: (A, T) => A) = accu // a reparer
  def reduceTree[A](accu: A, fun: (A, T, A) => A) = accu // a reparer
  def fold[A](accu: A, fun: (T, LNode[A]) => A) = fun (value, cons.lmap({x => x.fold(accu, fun)}))
  def twoFold[A](accu: A, fun: (T, A, A) => A) = accu // a reparer
}

case class Node[T](val value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
//  override def toString = "Node(" + value.toString + ", " + left.toString + ", " + right.toString + ")"
  override def toString2 = "Node wtf"
  override def toString = this match {
    case Node(value, a: Node[T], b: Node[T]) => "Node(" + value.toString + ", " + a.toString + ", " + b.toString + ")"
    case Node(value, a: Node[T], b: Nill[T]) => "Node(" + value.toString + ", " + a.toString + ")"
    case Node(value, b: Nill[T], a: Node[T]) => "Node(" + value.toString + ", " + a.toString + ")"
    case Node(value, a: Nill[T], b: Nill[T]) => value.toString
    case Node(a, b, c) => a + ", " + b + ", " + c
    case _ => "Node case wtf"
  }
  def this(value: T) = this(value, new Nill[T], new Nill[T])
  def this(value: T, left: Tree[T]) = this(value, left, new Nill[T])
  def map[A](f: (T) => A): Node[A] = new Node(f(value), left.map(f), right.map(f))
  def foldFlat[A](accu: A)(fun: (A, T) => A): A = right.foldFlat(left.foldFlat(fun(accu, value))(fun))(fun)
//  def reduceTree[A](accu: A, fun: (A, T, A) => A) = fun (left.reduceTree(accu, fun), value, right.reduceTree(accu, fun))
//  def fold[A](accu: A, fun: (T, List[A]) => A) = fun (value, List(left.fold(accu, fun), right.fold(accu, fun)))
  def fold[A](accu: A, fun: (T, LNode[A]) => A) = fun (value, LNode(left.fold(accu, fun), LNode(right.fold(accu, fun))))
//  def fold[A](accu: A, fun: (T, LNode[A]) => A) = fun(value, LNode(left.fold(accu, fun),  
  def twoFold[A](accu: A, fun: (T, A, A) => A) = fun (value, left.twoFold(accu, fun), right.twoFold(accu, fun))
}


/*
case class nNode[T](val value: T, branches: List[Tree[T]]) extends Tree[T] {
  def this(value: T) = this(value, Nil)
  override def toString = branches match {
    case head :: tail => "nNode( " + value + ": " + branches.mkString(", ") + ")"
    case Nil => value.toString
  }
  def map[A](f: (T) => A): nNode[A] = new nNode(f(value), branches.map(_.map(f)))
  def foldFlat[A](accu: A)(fun: (A, T) => A): A = accu // a reparer
  def reduceTree[A](accu: A, fun: (A, T, A) => A) = accu
  def fold[A](accu: A, fun: (T, List[A]) => A) = fun (value, branches.map({x => x.fold(accu, fun)}))
  def twoFold[A](accu: A, fun: (T, A, A) => A) = {
    def rec(list: List[Tree[T]]): A= list match {
      case Nil => accu
      case tete :: Nil => fun(value, tete.twoFold(accu, fun), accu)
      case tete :: second => fun(value, tete.twoFold(accu, fun), rec(second))
    }
    rec(branches)
  }
//  def flatten(f: (T, T) => Boolean) = this
}
*/

/*
val testNums = new Node(1, new Node(2), new Node(3, new Node(4)))
val testNums2 = new Node(1, new Node(5, new Node(2), new Node(3)), new Node(4))
val testNums3 = new Node(1, new NNode(
// identity fold
*/

