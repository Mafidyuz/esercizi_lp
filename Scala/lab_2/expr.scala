import scala.util.parsing.combinator._
import scala.io.Source



3+4*5
4*5+3
class ExpressionParser extends JavaTokenParsers {
    def expr: Parser[Any] = num ~ op1 ~ expr  
    def num = decimalNumber ^^ {_.toDouble}
    def op1 = ("+"|"-"|"*"|"/") ^^ {_.charAt(0)} 
    def op2 = "sqrt"|"sin"|"cos"|"tan"
}

val p = new ExpressionParser
p.parseAll(p.expr, "3*4+2-3") match {
    case p.Success((r:Expr),_) => r
    case _ => 
}