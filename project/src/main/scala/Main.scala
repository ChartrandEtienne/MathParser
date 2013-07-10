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
      println("\tvariables:\t" + powerSerie.getVariables + "\n")
    }
  } catch {
    case e => println("what the flying fuck: " + e.getMessage)
  }
}
