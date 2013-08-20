package parsing

import scala.io.Source
import scala.util.matching.Regex

object Main extends App {
  println("hello, world")
//  ExprVec.wolframTests
  val comma = ",".r
//  var line2 = "x + 2"
  try {
    for(line <- Source.fromFile("expr").getLines()) {
      println("\n\n\n\tsource :\t" + line)  

      val expr = p.apply(line)
      println("\texpr:\t"+expr.toString+"\n")
                              
//      println(  new Expression(p.apply(line).toTree).toFlatten.simplify )
//      println(  (new Expression(p.apply(line).toTree).toFlatten.simplify).toVectorRep )
//      val vector1 = (new Expression(p.apply(line).toTree).toFlatten.simplify).toVectorRep 
//                              
////      println("\nvec:\t"+vector1.toString)
//      val u = new ExprVec(  vector1.expr.map(_ / vector1.expr.last))
//      println("\n\tu:\t"+u.toString)
//      val p0 = List(Real(1))
//      println("\n\tp0:\t"+p0.toString)
//      val r = (u / u.deriv)._1
//      println("\n\tr:\t"+r.toString)
//      val f = (u / r)._1
//      println("\n\tf:\t"+f.toString)
        
    }
  } catch {
    case e => println("what the flying fuck: " + e.getMessage)
  }
}

