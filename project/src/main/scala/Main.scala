package parsing

object Main extends App {
  println("hello, world")
  val pp = p.apply(_: String).toTree
  val testExpr = p.apply("9+2x^2+2x+y+8+r")
  val testTree = testExpr.toTree
  val testExpr2 = p.apply("4x^4-5x")
  val simple = new Expression(p.apply("3+4+5").toTree)
  val testTree2 = testExpr2.toTree
  val Expr = new Expression(testTree)
  val Expr2 = new Expression(testTree2)
  val testLNode = LNode(1, 2, 3, 4)
  println("\n\ttestLNode: " + testLNode.toString)
//  val testNNode = NNode(1, LNode(2, 3, 4, 5))
//  println("\n\ttestNNode: " +testNNode.toString)
  try {
    println("attention les yeux")
    val test = Expr.toPowerSeries.toString
    println("\n\taavant:\t9+2x^2+2x+y+8+r")
    println("\n\tavant:\t" + testTree.toString)
    println("\n\tapres:\t" + test)
    println("\n\n\tsimple:\n\taavant:\t3+4+5\n\tavant: " + simple.toString + "\n\tapres: " + simple.toPowerSeries.toString)
  } catch {
    case e => println("what the flying fuck: " + e.getMessage)
  }
}
