import scala.util.parsing.combinator._

//((2 + 7) + ((3 + 9) + 4))

trait Expr{
    def eval: Int
    def isSimple: Boolean
}
case class Sum(a:Expr, b:Expr) extends Expr {
    def eval = a.eval + b.eval
    def isSimple = a.isInstanceOf[Num] && b.isInstanceOf[Num]
    override def toString = "(%s+%s)".format(a.toString,b.toString)
}
case class Sub(a:Expr, b:Expr) extends Expr {
    def eval = a.eval - b.eval
    def isSimple = a.isInstanceOf[Num] && b.isInstanceOf[Num]
    override def toString = "(%s-%s)".format(a.toString,b.toString)
}
case class Mul(a:Expr, b:Expr) extends Expr {
    def eval = a.eval * b.eval
    def isSimple = a.isInstanceOf[Num] && b.isInstanceOf[Num]
    override def toString = "(%s*%s)".format(a.toString,b.toString)
}
case class Div(a:Expr, b:Expr) extends Expr {
    def eval = a.eval / b.eval
    def isSimple = a.isInstanceOf[Num] && b.isInstanceOf[Num]
    override def toString = "(%s/%s)".format(a.toString,b.toString)
}
case class Num(n:Int) extends Expr {
    def eval = n
    def isSimple = true
    override def toString = n.toString
} 

case class Program(var expression: Expr) {
    def eval(e: Expr): Expr = {
        e match {
            case s: Sum => if(s.isSimple) Num(s.eval) else Sum(eval(s.a), eval(s.b))
            case s: Sub => if(s.isSimple) Num(s.eval) else Sub(eval(s.a), eval(s.b))
            case s: Mul => if(s.isSimple) Num(s.eval) else Mul(eval(s.a), eval(s.b))
            case s: Div => if(s.isSimple) Num(s.eval) else Div(eval(s.a), eval(s.b)) 
            case n: Num => n
        }
    }

    def exec = {
        var e = expression
        while(!e.isInstanceOf[Num]){
            println(e.toString)
            e = eval(e)
        }
        println(e.toString)
    }

}

class ArithmeticParser extends JavaTokenParsers{
    def program = expr ^^ {case e => Program(e)}
    def expr: Parser[Expr] = ("(" ~> expr ~ op ~ expr <~ ")" | num)  ^^ {
        case n:Int => Num(n)
        case (e1: Expr) ~ "+" ~ (e2: Expr) => Sum(e1,e2)
        case (e1: Expr) ~ "-" ~ (e2: Expr) => Sub(e1,e2)
        case (e1: Expr) ~ "*" ~ (e2: Expr) => Mul(e1,e2)
        case (e1: Expr) ~ "/" ~ (e2: Expr) => Div(e1,e2)
    }
    def num = "[0-9]+".r ^^ {_.toInt}
    def op = "+"|"-"|"*"|"/"
}


class WrongExpressionException extends RuntimeException

object StepByStepEvaluator {
    def main(args: Array[String]) {
        val p = new ArithmeticParser
        args.foreach{ expr => 
            p.parseAll(p.program, expr) match {
                case p.Success(res, _) => res.exec; println()
                case _ => throw new WrongExpressionException
            }
        }
    }
}
//scala StepByStepEvaluator "((2 + 7) + ((3 + 9) + 4))" "((1 * 7) + (7 * ((3 + 9) + 5)))" "((5*(7-2))+(((15/3)+2)-((1422*2)-(3500/4))))"