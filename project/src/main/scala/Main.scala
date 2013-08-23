package parsing

import scala.io.Source

object Main extends App {
  println("hello, world")
  try {
    for(line <- Source.fromFile("expr").getLines()) {
      println("\tsource:\t"+line)
      val expr = new Expression(p.apply(line))
      println("\texpr:\t"+expr.toString)
      val expr2 = expr.flat
      println("\texpr2:\t"+expr2.toString+"\n")
    }  
  } catch {
    case e => println("what the flying fuck: " + e.getMessage)
  }
}

