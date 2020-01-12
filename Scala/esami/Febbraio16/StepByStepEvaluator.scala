import scala.util.parsing.combinator._
import scala.io.Source

trait Expr {
    def eval: Int
}

class Num(n: Int) extends Expr {
    def eval: Int = n
    override def toString: String =  n.toString
}

class Sum(A: Expr, B: Expr) extends Expr {
    val a = A
    val b = B
    def eval: Int = a.eval + b.eval
    override def toString: String = "(" + a.toString + "+" + b.toString + ")"
}

class Sub(A: Expr, B: Expr) extends Expr {
    val a = A
    val b = B
    def eval: Int = a.eval - b.eval
    override def toString: String = "(" + a.toString + "-" + b.toString + ")"
}

class Mul(A: Expr, B: Expr) extends Expr {
    val a = A
    val b = B
    def eval: Int = a.eval * b.eval
    override def toString: String = "(" + a.toString + "*" + b.toString + ")"
}

class Div(A: Expr, B: Expr) extends Expr {
    val a = A
    val b = B
    def eval: Int = a.eval / b.eval
    override def toString: String = "(" + a.toString + "/" + b.toString + ")"
}



class ArithmeticParser extends JavaTokenParsers {
    def expr:ArithmeticParser.this.Parser[Any] = ("(" ~> expr ~ op ~ expr <~ ")" | number) ^^ {
        case (a:Expr) ~ "+" ~ (b:Expr) => new Sum(a,b)
        case (a:Expr) ~ "-" ~ (b:Expr) => new Sub(a,b)
        case (a:Expr) ~ "*" ~ (b:Expr) => new Mul(a,b)
        case (a:Expr) ~ "/" ~ (b:Expr) => new Div(a,b)
        case n:Expr => n
    }
    def number = "-?[0-9]+".r ^^ {x => new Num(x.toInt)}
    def op = "+" | "-" | "*" | "/"

}
// scala StepByStepEvaluator "((2 + 7) + ((3 + 9) + 4))" "((1 * 7) + (7 * ((3 + 9) + 5)))" "((5*(7-2))+(((15/3)+2)-((1422*2)-(3500/4))))"

object StepByStepEvaluator  
{ 
    def evaluator(e: Expr): Expr = e match {
        case x:Sum => (x.a, x.b) match {
            case(a:Num, b:Num)=> new Num(a.eval + b.eval)
            case(a:Expr,b:Expr) => new Sum(evaluator(a), evaluator(b))
        }
        case x:Sub => (x.a, x.b) match {
            case(a:Num, b:Num)=> new Num(a.eval - b.eval)
            case(a:Expr,b:Expr) => new Sub(evaluator(a), evaluator(b))
        }
        case x:Mul => (x.a, x.b) match {
            case(a:Num, b:Num)=> new Num(a.eval * b.eval)
            case(a:Expr,b:Expr) => new Mul(evaluator(a), evaluator(b))
        }
        case x:Div => (x.a, x.b) match {
            case(a:Num, b:Num)=> new Num(a.eval / b.eval)
            case(a:Expr,b:Expr) => new Div(evaluator(a), evaluator(b))
        }    
        case x:Num => x
    }

    def main(filename: Array[String]){ 
        println()
        val parser = new ArithmeticParser
        filename.foreach(s => 
            parser.parseAll(parser.expr, s) match {
                case parser.Success((r:Expr),_) =>
                    println(r)
                    var e = r
                    while(!e.isInstanceOf[Num]){
                        println(evaluator(e))
                        e = evaluator(e)
                    }
                    println()
                case _ => "errore"
            }
        )
    } 
} 