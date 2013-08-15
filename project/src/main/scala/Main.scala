package parsing

import scala.io.Source

object Main extends App {
  println("hello, world")
  var line2 = "x + 2"
  try {
    for(line <- Source.fromFile("expr").getLines()) {
      println("\n\n\n\tsource :\t" + line)  
      println("\tsource2:\t" + line2)  
      var exprTree = new Expression(p.apply(line).toTree)
      println("\tpreflat:\t" + exprTree)
      val exprFlattened = exprTree.toFlatten
      println("\tpresimp:\t" + exprFlattened)
      var powerSerie = exprFlattened.simplify
      var exprTree2 = new Expression(p.apply(line2).toTree)
      var powerSerie2 = exprTree2.toFlatten.simplify
//      println("\tdeg:\t" + powerSerie.degPowerSeries)
      val vector1 = powerSerie.toVectorRep
      val vector2 = powerSerie2.toVectorRep
      println("\tvec:\t\t" + vector1)
//      println("\tvev2:\t\t" + vector2)
//      println("\tsum:\t\t" + (vector1 + vector2).toString)
//      println("\t\t" + vector1.toString + " // " + vector2.toString)
//      val div1 = vector1 / vector2
//      println("\tdiv:\t\t" + div1.toString)
//      val gcd1 = vector1 gcd vector2
//      println("\tgcd:\t\t" + gcd1.toString)
      val deriv1 = vector1.deriv
      println("\tderiv:\t\t" + deriv1.toString)
      val gcd1 = vector1 gcd deriv1
      println("\tgcd:\t\t" + gcd1.toString)
      
  
/*
      val mult = vector1 * vector2
      println("\tmult:\t\t" + mult.toString)
      println("\tmult:\t\t" + (mult / vector2).toString)
*/
      line2 = line
    }
  } catch {
    case e => println("what the flying fuck: " + e.getMessage)
  }
}

