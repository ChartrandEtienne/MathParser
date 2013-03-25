package com.parser
import scala.util.parsing.combinator._
// import scala.util.matching.Regex
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

sealed abstract class Expr {
	def eval: Int
	def _toString: String
}

case class EVar(value: String) extends Expr {
	def eval = 0
	def _toString = value
}

case class EConst(value: Int) extends Expr {
	def eval(): Int = value
	def _toString(): String = eval.toString
}

case class EAdd(left: Expr, right: Expr) extends Expr {
	def eval(): Int = left.eval + right.eval
	def _toString(): String = "(" + left._toString + " + " + right._toString + ")"
}

case class ESub(left: Expr, right: Expr) extends Expr {
	def eval(): Int = left.eval - right.eval
	def _toString(): String = "(" + left._toString + " - " + right._toString + ")"
}

case class EMult(left: Expr, right: Expr) extends Expr {
	def eval(): Int = left.eval * right.eval
	def _toString(): String = "(" + left._toString + " * " + right._toString + ")"
}

case class EDiv(left: Expr, right: Expr) extends Expr {
	def eval(): Int = (left.eval / right.eval).toInt
	def _toString(): String = "(" + left._toString + " / " + right._toString + ")"
}

case class EProd(a: List[Expr]) extends Expr {
	def eval(): Int = a.foldLeft(1)(_ * _.eval)
	def _toString(): String = "eh"
}

case class ESum(a: List[Expr]) extends Expr {
	def eval(): Int = a.foldLeft(0)(_ + _.eval) 
	def _toString(): String = "uh"
}


object mParser extends StandardTokenParsers {
	lexical.delimiters ++= List("+")
	
	def value = numericLit ^^ {s => EConst(s.toInt)}


	def sum = value ~ "+" ~ value ^^ {case left ~ "+" ~ right => EAdd(left, right)}

	def expr = (sum|value)

	def parse(s: String) = {
		val tokens = new lexical.Scanner(s)
		val parserFinal = phrase(expr)
		parserFinal(tokens)	
	//	phrase(expr)(tokens)
	}

	def apply(s: String) = parse(s) match {
		case Success(tree, _) => tree
		case e: NoSuccess =>
			throw new IllegalArgumentException("Bad syntax: " + s)
	}
		
}
	

/*

object ExprParser extends StandardTokenParsers {
	lexical.delimiters ++= List("*", "/", "+", "-", "(", ")")
	
	def value = numericLit ^^ {s => EConst(s.toInt)}


//	def variable: Parser[EVar] = """[a-z]""".r ^^ {s => EVar(s)}

///	def sum = value ~ "+" ~ value ^^ {case left ~ "+" ~ right => 
//		EAdd(left, right) }

//	def sum = repsep(value, "+") ^^ {a: List[Expr] => ESum(a) }

	def parens: Parser[Expr] = "(" ~> expr <~ ")"

	def term = (value|variable|parens)

	def prod = value*(
		"*" ^^^ {(a: Expr, b: Expr) => EMult(a, b)}|
		"/" ^^^ {(a: Expr, b: Expr) => EDiv(a, b)})

	def sum = prod*(
		"+" ^^^ {(a: Expr, b: Expr) => EAdd(a, b)}|
		"-" ^^^ {(a: Expr, b: Expr) => ESub(a, b)} )

//	def sub = repsep(value, "-") ^^ {a: List[Expr] => ESub(a) }
	
	def expr = (sum|value)	

	def parse(s: String) = {
		val tokens = new lexical.Scanner(s)
		phrase(expr)(tokens)
	}

	def apply(s: String): Expr = {
		parse(s) match {
			case Success(tree, _) => tree
			case e: NoSuccess =>
				throw new IllegalArgumentException("Bad syntax: " + s)
		}
	}
}
*/

	
