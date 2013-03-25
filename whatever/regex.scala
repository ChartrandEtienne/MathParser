import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.matching.Regex
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.collection.mutable


sealed abstract class Expr {
	def eval: Int
	def _toString: String
}

case class EError(value: String) extends Expr {
	def eval = -1
	def _toString = value
}

case class EVar(value: String) extends Expr {
	def eval = 0
	def _toString = value
}

case class EConst(value: Int) extends Expr {
	def eval(): Int = value
	def _toString = eval.toString
}

case class EAdd(left: Expr, right: Expr) extends Expr {
	def eval(): Int = left.eval + right.eval
	def _toString = "(" + left._toString + " + " + right._toString + ")"
}

case class ESub(left: Expr, right: Expr) extends Expr {
	def eval(): Int = left.eval - right.eval
	def _toString = "(" + left._toString + " - " + right._toString + ")"
}

case class EMult(left: Expr, right: Expr) extends Expr {
	def eval(): Int = left.eval * right.eval
	def _toString = "(" + left._toString + " * " + right._toString + ")"
}

case class EDiv(left: Expr, right: Expr) extends Expr {
	def eval(): Int = (left.eval / right.eval).toInt
	def _toString = "(" + left._toString + " / " + right._toString + ")"
}

case class EPow(left: Expr, right: Expr) extends Expr {
	def eval(): Int = 0 // whatever
	def _toString = left._toString + "^" + right._toString
}


import scala.util.parsing.syntax.StdTokens


trait MathTokens extends StdTokens {
  case class Num(chars: String) extends Token {
    override def toString = "`"+chars+"`"
  }
}

class MathLexical extends Lexical with MathTokens {
  def token: Parser[Token] = rep(digit) ^^ {case num => Num(num mkString "")}
  def whitespace: Parser[Any] = rep[Any](
      whitespaceChar
//    | '/' ~ '*' ~ comment
    | '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
    | '/' ~ '*' ~ failure("unclosed comment")
    )
}

class MathTokenParsers extends TokenParsers {
  type Tokens = MathTokens
  val lexical = new MathLexical
  def numericLit: Parser[String] = 
    elem("number", _.isInstanceOf[Tokens]) ^^ (_.chars) 
} 

import scala.util.parsing.combinator.syntactical._

object MathParser extends MathTokenParsers {
//   override val lexical = new MathLexical
  // type Tokens = MathTokens
  def value = numericLit ^^ {s => EVar(s.toString)}
  def expr = value
  def apply(s: String): Expr = {
    parse(s) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
             throw new IllegalArgumentException("Bad syntax: "+s)
    }
  }
  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(expr)(tokens)
  }
}
  


/*
object bnf extends RegexParsers {

	def equation = sum

	def sum: Parser[Expr] = product~("+"~product|"-"~product)*
	def product: Parser[Expr] = term~("*"~term|"/"~term)*
	def term: Parser[Expr]


	def apply(s: String): Expr = parseAll(S, s) match {
		case Success(result, _) => result
		case failure : NoSuccess => EError(failure.msg)
	}
}
*/
	
/*
class mathLexical extends StdLexical {

	def chr(c: Char) = elem("", ch => ch == c)

	override def token: Parser[Token] = operator|value
	
	def operator: Parser[Token] = chr("+")|chr("-")

	//def value: Parser[Token] = """[a-z]""".r ^^ {s => s}
}	
*/


/*
class MathLexical2 extends StdLexical {
  override def token: Parser[Token] = superToken | super.token

  def superToken = IntLit | Variable | Operator
  
  def IntLit: Parser[Token] = 
    rep1(digit) ^^ {case number => NumericLit(number mkString "")}

  def Variable: Parser[Token] = letter

  def chr(c:Char) = elem("", ch => ch==c )

  def Operator: Parser[Token] = ('+' | '-' | '*' | '/' | '^') ^^ {case op => StringLit(op)}

//   def Operator: Parser[Token] = (chr('+') | chr('-') | chr('*') | chr('/') |chr('^')) ^^ {case op => StringLit(op)}
} 
*/

    

/*
class MathLexical extends Lexical with StdTokens {
  def token: Parser[Token] = 
    (digit~rep(digit) ^^ {case first~rest => NumericLit(first::rest mkString "")})

  def whitespace: Parser[Any] = rep[Any](
      whitespaceChar
//    | '/' ~ '*' ~ comment
    | '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
    | '/' ~ '*' ~ failure("unclosed comment")
    )
}
*/


/*
object ExprParser extends StandardTokenParsers {
  override val lexical = new MathLexical2
}	
*/

/*

object Wikipedia extends RegexParsers {

	def variable = """[a-z]""".r ^^ {s => EVar(s)}
	
	def T: Parser[Expr] = (
		variable|
		"("~>S<~")")

	def S: Parser[Expr] = (
		T~"/"~S ^^ {case x~"/"~y => EDiv(x, y)}|   
		T~"^"~S ^^ {case x~"^"~y => EPow(x, y)}|
		T~"+"~S ^^ {case x~"+"~y => EAdd(x, y)}|
		T~"-"~S ^^ {case x~"-"~y => ESub(x, y)}|
		T~"*"~S ^^ {case x~"*"~y => EMult(x, y)}|
		T)

	def apply(s: String): Expr = parseAll(S, s) match {
		case Success(result, _) => result
		case failure : NoSuccess => EError(failure.msg)
	}
}
*/

/*

object McBeathDerive extends RegexParsers {
//	override val skipWhitespace = true
	def litteral = """\d+""".r ^^ {s => EConst(s.toInt)}

	def variable = """[a-z]""".r ^^ {s => EVar(s)}

	def parens: Parser[Expr] = "("~>expr<~")"

	def value = litteral | variable
	
	def term = value | parens

	def expr = sum | term 

/*
	def exponent: Parser[Expr] = (
		value~"^"~value ^^ {case x~"^"~y => EPow(x, y)}|
		value)

	def product: Parser[Expr] = (
		term~expr^^ {case x ~ y => EMult(x, y)}|
		value)

	def product: Parser[Expr] = (
		exponent~term ^^ {case x ~ y => EMult(x, y)}|
		term)							
*/

/*
	def exponent: Parser[Expr] = (
		term~"^"~value^^ {case x ~"^"~ y => EPow(x, y)}|
		term)

	def product: Parser[Expr] = (
		exponent~exponent^^ {case a~b => EMult(a, b)}|
		term)

//		term~"^"~value^^ {case x ~"^"~ y => EPow(x, y)}|	
//		term~expr ^^ {case x ~ y => EMult(x, y)}|

	

	def sum: Parser[Expr] = product*(
		"+" ^^^ {(a: Expr, b: Expr) => EAdd(a,b)}|
		"-" ^^^ {(a: Expr, b: Expr) => ESub(a,b)})

	def apply(s: String): Expr = parseAll(expr, s) match {
		case Success(result, _) => result
		case failure : NoSuccess => EError(failure.msg)
	}
}
*/


/*
object bnf extends RegexParsers {

	def equation = sum

	def sum: Parser[Expr] = product~("+"~product|"-"~product)*
	def product: Parser[Expr] = term~("*"~term|"/"~term)*
	def term: Parser[Expr]


	def apply(s: String): Expr = parseAll(S, s) match {
		case Success(result, _) => result
		case failure : NoSuccess => EError(failure.msg)
	}
}
*/
	
/*
class mathLexical extends StdLexical {

	def chr(c: Char) = elem("", ch => ch == c)

	override def token: Parser[Token] = operator|value
	
	def operator: Parser[Token] = chr("+")|chr("-")

//	def value: Parser[Token] = """[a-z]""".r ^^ {s => s}
}	

object ExprParser extends StandardTokenParsers {
	override val lexical = new mathLexical
}	

object Wikipedia extends RegexParsers {

	def variable = """[a-z]""".r ^^ {s => EVar(s)}
	
	def T: Parser[Expr] = (
		variable|
		"("~>S<~")")

	def S: Parser[Expr] = (
		T~"/"~S ^^ {case x~"/"~y => EDiv(x, y)}|   
		T~"^"~S ^^ {case x~"^"~y => EPow(x, y)}|
		T~"+"~S ^^ {case x~"+"~y => EAdd(x, y)}|
		T~"-"~S ^^ {case x~"-"~y => ESub(x, y)}|
		T~"*"~S ^^ {case x~"*"~y => EMult(x, y)}|
		T)

	def apply(s: String): Expr = parseAll(S, s) match {
		case Success(result, _) => result
		case failure : NoSuccess => EError(failure.msg)
	}
}
*/

/*

object McBeathDerive extends RegexParsers {
//	override val skipWhitespace = true
	def litteral = """\d+""".r ^^ {s => EConst(s.toInt)}

	def variable = """[a-z]""".r ^^ {s => EVar(s)}

	def parens: Parser[Expr] = "("~>expr<~")"

	def value = litteral | variable
	
	def term = value | parens

	def expr = sum | term 

/*
	def exponent: Parser[Expr] = (
		value~"^"~value ^^ {case x~"^"~y => EPow(x, y)}|
		value)

	def product: Parser[Expr] = (
		term~expr^^ {case x ~ y => EMult(x, y)}|
		value)

	def product: Parser[Expr] = (
		exponent~term ^^ {case x ~ y => EMult(x, y)}|
		term)							
*/

/*
	def exponent: Parser[Expr] = (
		term~"^"~value^^ {case x ~"^"~ y => EPow(x, y)}|
		term)

	def product: Parser[Expr] = (
		exponent~exponent^^ {case a~b => EMult(a, b)}|
		term)

//		term~"^"~value^^ {case x ~"^"~ y => EPow(x, y)}|	
//		term~expr ^^ {case x ~ y => EMult(x, y)}|

	

	def sum: Parser[Expr] = product*(
		"+" ^^^ {(a: Expr, b: Expr) => EAdd(a,b)}|
		"-" ^^^ {(a: Expr, b: Expr) => ESub(a,b)})

	def apply(s: String): Expr = parseAll(expr, s) match {
		case Success(result, _) => result
		case failure : NoSuccess => EError(failure.msg)
	}
}
*/

