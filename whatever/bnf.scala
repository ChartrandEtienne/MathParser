import scala.util.parsing.combinator._


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

object p extends RegexParsers {
	
	def variable = """[a-z]""".r ^^ {s => EVar(s)}
	
	def T: Parser[Expr] = variable

	def S: Parser[Expr] = (
		S~"+"~T |
		S~"-"~T)

	def apply(s: String): Expr = parseAll(expr, s) match {
		case Success(result, _) => result
		case failure : NoSuccess => EError(failure.msg)
	}
}


