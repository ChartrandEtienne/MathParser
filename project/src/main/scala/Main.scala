package parsing

import scala.io.Source

object Main extends App {
  println("hello, world")
  try {
    for(line <- Source.fromFile("expr").getLines()) {
      val expr = p.apply(line)
      println("\texpr:\t"+expr.toString+"\n")
    }  
  } catch {
    case e => println("what the flying fuck: " + e.getMessage)
  }
}

