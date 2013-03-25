import implicitTerm._

object implicitTerm {
  implicit def string2Var(s: String) = new VariableNode(s)
  implicit def string2Num(s: Int) = new NumeralNode(s)
//  implicit def list2PowerSeries(s: List[Term]) = new PowerSeries(s)
//  implicit def term2PowerSeries(s: Term) = new PowerSeries(List(s))
}

case class Term(coefficient: NumeralNode, variable: List[(VariableNode, NumeralNode)]) {
  override def toString = coefficient.toString + variable.mkString("")
  def +(that: Term): Option[Term] = {
    if (that.variable == this.variable) Some(Term(this.coefficient + that.coefficient, that.variable))
    else None
  }
  def -(that: Term): Option[Term] = {
    if (that.variable == this.variable) Some(Term(this.coefficient - that.coefficient, that.variable))
    else None
  }
}


// dans un avenir lointain
// remplacer par un truc qui sous-classe List. 
// pour pouvoir profiter du polymorphisme ou whatever.
class PowerSeries(terms: List[Term]) {
  def addTermHelp(them: List[Term], dat: Term): List[Term] = dat + them.head match {
    case Some(x: Term) => x :: them.tail
    case None => if (them.tail == Nil) them.head :: dat :: Nil else them.head :: addTermHelp(them.tail, dat)
  } 
  def simplifyHelp(tree: List[Term], fruit: List[Term]): List[Term] = 
    fruit.foldLeft(tree) {(seed, x) => {
      addTermHelp(seed, x) }
    }
  override def toString = terms.mkString(" + ")
  def b = terms
  def addTerm(that: Term) = new PowerSeries(addTermHelp(terms, that))
  def simplify = new PowerSeries(simplifyHelp(List(this.terms.head), this.terms.tail))
  def +(that: PowerSeries) = new PowerSeries(simplifyHelp(that.b, this.terms))
  def *(that: PowerSeries) = this // magic 
}  

def walker(tree: MathNode): List[MathNode] = tree match {
  case BinOpNode(x, y, "+") => y :: walker(x) 
  case z: BinOpNode => List(z)
  case _ => Nil }

// algo plutot stupide: requiert une entree de la forme 2x^2
// surement un moyen de beautifuller ca sans just tous les matcher a la main (yuck)

// ne reconnait aussi que les formes power series, et dont chaque terme n'a qu'une seule varible. Potentiellement retaper l'achitecture de
// merde. 
def getTerm(terme: MathNode): Term = terme match {
  case x: NumeralNode => new Term(x, List(("a", 1)))
  case x: VariableNode => new Term(1, List((x, 1)))
  case BinOpNode(l: NumeralNode, r: VariableNode, _) => new Term(l, List((r, 1)))
  case BinOpNode(l: NumeralNode, BinOpNode(b: VariableNode, e: NumeralNode, _), _) => new Term(l, List((b, e)))
  case _ => null
}

def flatten(node: MathNode): List[MathNode] = node match {
  case BinOpNode(x, y, "*") => y :: flatten(x)
  case BinOpNode(x, y, _) => x :: y :: Nil
  case x: MathNode => x :: Nil
  case _ => Nil
}

def getFlattenedTerm(n: List[MathNode]): Term = n match {
//   def helper(termes: List[MathNode]): Term = termes match {
    case (x: NumeralNode) :: xs => Term(x, xs.foldLeft(List[(VariableNode, NumeralNode)]()) { (seed, fruit) => fruit match {
      case x: VariableNode => (x, NumeralNode(1)) :: seed
      case BinOpNode(x: VariableNode, y: NumeralNode, "^") => (x, y) :: seed }})
    case _ => Term(1, Nil)
//     }
//   helper(n.reverse)
}
  

def getTerms(liste: List[MathNode]): List[Term] = liste.foldLeft(List[Term]()) {
  (seed, x) => getTerm(x) :: seed}

val testExpr = p.apply("2x^2+2x+y+8")
val testList = walker(testExpr)
val testPower = new PowerSeries(getTerms(testList))
val testPlus = new PowerSeries(getTerms(walker(p.apply("x+y+2x+2y+3x+3y"))))
val power = walker(p.apply("2x^2y+3yz^2")).map(flatten(_)).map(_.reverse).map(getFlattenedTerm(_))

