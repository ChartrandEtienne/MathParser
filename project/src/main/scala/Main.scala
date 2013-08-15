package parsing

import scala.io.Source

object Main extends App {
  println("hello, world")
  var line2 = "x + 2"
  try {
    for(line <- Source.fromFile("expr").getLines()) {
      println("\n\tsource :\t" + line)  
      println("\tsource2:\t" + line2)  
      var exprTree = new Expression(p.apply(line).toTree)
      println("\tpreflat:\t" + exprTree)
      val exprFlattened = exprTree.toFlatten
      println("\tpresimp:\t" + exprFlattened)
      var powerSerie = exprFlattened.simplify
      var exprTree2 = new Expression(p.apply(line2).toTree)
      var powerSerie2 = exprTree2.toFlatten.simplify
//      println("\tdeg:\t" + powerSerie.degPowerSeries)
      println("\tvec:\t\t" + powerSerie.toVectorRep)
      println("\tvev2:\t\t" + powerSerie2.toVectorRep) 
      println("\tsum:\t\t" + (powerSerie.toVectorRep + powerSerie2.toVectorRep).toString)
      println("\tdiv:\t\t" + (powerSerie2.toVectorRep / powerSerie.toVectorRep).toString)
      line2 = line
    }
  } catch {
    case e => println("what the flying fuck: " + e.getMessage)
  }
}

