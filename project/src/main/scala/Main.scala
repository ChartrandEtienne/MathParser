package parsing

import scala.io.Source

object Main extends App {
  println("hello, world")
  try {
    for(line <- Source.fromFile("expr").getLines()) {
      println("\n\tsource:\t" + line)  
      var exprTree = new Expression(p.apply(line).toTree)
      println("\tarbre:\t" + exprTree.toString)
      var powerSerie = exprTree.toPowerSeries
      println("\tpower:\t" + powerSerie.toString + "\n")
    }
  } catch {
    case e => println("what the flying fuck: " + e.getMessage)
  }
}
