package parsing

import scala.io.Source

object Main extends App {
  println("hello, world")
  try {
    for(line <- Source.fromFile("expr").getLines()) {
      println("\n\tsource:\t" + line)  
      var exprTree = new Expression(p.apply(line).toTree)
      println("\ttree:\t" + exprTree.toString)
      var powerSerie = exprTree.toFlatten
      println("\tflat:\t" + powerSerie.toString)
      println("\tpower:\t" + powerSerie.isPowerSeries)
      println("\tvars:\t" + powerSerie.getVariables.mkString(", "))
      println("\teval:\t" + powerSerie.eval(Map("x" -> 4))+ "\n")
      
    }
  } catch {
    case e => println("what the flying fuck: " + e.getMessage)
  }
}
