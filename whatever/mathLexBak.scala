import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.syntax.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.collection.mutable
import scala.util.matching.Regex
import java.util.regex.Pattern
import scala.util.parsing.combinator._

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


abstract class MathToken  {
}

case class failToken

case class nombreToken(value: String) extends MathToken {
  override def toString = value.toString
}

case class variableToken(value: String) extends MathToken {
  override def toString = value
}

case class operatorToken(value: String) extends MathToken {
  override def toString = value
}

case class openParenToken extends MathToken {
}

case class closeParenToken extends MathToken {
}
  

class MathLexer extends RegexParsers {
  def number: Parser[nombreToken] = regex(new Regex("[0-9]+")) ^^ {nombreToken(_)} // "
  def variable: Parser[variableToken] = regex(new Regex("[a-z]")) ^^ {variableToken(_)} // "
  def operator: Parser[operatorToken] = regex(new Regex("[/*/^//+-]")) ^^ {operatorToken(_)} //"
  def openParens: Parser[openParenToken] = regex(new Regex("[/(]")) ^^ {_ => new openParenToken} // "
  def closeParens: Parser[closeParenToken] = regex(new Regex("[/)]")) ^^ {_ => new closeParenToken} // "
  def exprLex = rep(number | variable | operator) 
  def applyLex(input: String): List[MathToken]= parse(exprLex, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => {println(failure.msg); Nil}
  }
}

abstract class MathNode

case class NumeralNode(value: Int) extends MathNode 

case class VariableNode(value: String) extends MathNode

case class BinOpNode(left: MathNode, right: MathNode, op: String) extends MathNode {
  override def toString = left.toString + " " + op + " " + right.toString
}

case class AddNode(left: MathNode, right: MathNode) extends MathNode 

/*
object p extends MathLexer {

  def litteral = number ^^ {x => {if (x.value == "0") NumeralNode(x.value.toInt) else if (x.value == "1") NumeralNode(1) else NumeralNode(2)}}
  def apply(input: String): MathNode = parse(litteral, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => {println(failure.msg); NumeralNode(-1)}
  }
}
*/

object p extends MathLexer {
  def litteral = number ^^ {x => NumeralNode(x.value.toInt)}
  def varLitteral = variable ^^ {x => VariableNode(x.value)}
  def value = litteral | varLitteral
  def term = value | parens
  // Essais en cours
  /*
  def binaryOp(level: Int): Parser[((Expr, Expr) => Expr)] = {
      if (case == 1) {
         
      case 1 =>
  // gnarly pattern-matching shit
  // m'entretuer avec acceptIf
*/
  def add: Parser[MathNode] = value ~ operator ~ value ^^ 
    {case x ~ y ~ z => {
      if (y.value == "+") AddNode(x, z)
      else failure("mauvais operateur")}}
        
/*
  def add = value ~ operator ~ value ^^
    {case x ~ y ~ z => BinOpNode(x, z, y.value)}
*/
  def expr = add | value
  def parens: Parser[MathNode] = openParens ~> expr <~ closeParens
  def apply(input: String): MathNode = parse(expr, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => {println(failure.msg); NumeralNode(-1)}
  }
}


/*
class MathParser extends Parsers {
  type Elem = MathToken
  val lexical = new MathLexer
  def binary: Parser[MathNode] = (nombreToken ~ operatorToken ~ nombreToken) ^^
    {case x ~ y ~ z => BinOpNode(NumeralNode(x.value.toInt), NumeralNode(z.value.toInt), y.value)}
}
*/

