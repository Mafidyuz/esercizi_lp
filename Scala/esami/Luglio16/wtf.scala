import scala.util.parsing.combinator._
import scala.io.Source
import java.util.Stack
import scala.collection.immutable.Map


class WTFParser(stack: Stack[Int], var functions: Map[Char, (Int, String)], var funArgs: Array[Int]) extends JavaTokenParsers{
    def program = definitions ~ calls
    override protected val whiteSpace = """[\ \t]+""".r
    def calls: Parser[Any] = repsep(( conditional | call), "[\n$]".r)
    def call: Parser[Any] = (rep(num | arg | fName | printOp )) ^^ { 
        ls => ls.foreach{x => x match {
            case n: Int => stack.push(n)
            case '!' => println(stack.pop)
            case f: Char => 
                val nArgs = functions(f)._1
                val body = functions(f)._2
                var args = List[Int]()
                for(i<-0 until nArgs) args = stack.pop :: args
                funArgs = args.toArray
                parseAll(calls, body)
                
            case _ => 
        }}
    }

    def parseBodyFun = rep(arg)
    def printOp = "!" ^^ {_.charAt(0)}
    def definitions = rep(defi) ^^ {x => functions = x.toMap}
    def defi = ("def" ~> fName ~ nArgs ~ ("=" ~> defiBody ) <~ "\n")^^ {
        case f ~ n ~ b => (f, (n,b))
    }
    def num = "0[+-]*".r ^^ {s => s.count(_ == '+') - s.count(_ == '-')}
    def fName = "[A-Z]".r ^^ {_.charAt(0)}
    def nArgs = "[0-9]+".r ^^ {_.toInt}
    def defiBody = ".*".r
    def conditional = (condition ~ ("?" ~> "[" ~> block <~ "]" <~ ":" <~ "[") ~ block <~ "]") ^^ {
        case c ~ b1 ~ b2 => 
            if(c == 0) parseAll(calls, b1)
            else parseAll(calls, b2)
    }
    def condition = num | arg
    def arg = "\\$[0-9]+[-+]?".r ^^ {s => funArgs(s.diff("$-+").toInt - 1) + s.count(_=='+') - s.count(_=='-') }
    def block = """[^\[\]]*""".r
    def fCall = fName 
}




val p = new WTFParser(new Stack[Int], Map[Char, (Int,String)](), new Array[Int](10))
val in = """def H 1 = $1 !
def S 2 = $2 ? [$1] : [$1+$2-S]
def M 2 = $2 ? [$1] : [$1-$2-M]
def A 3 = $2 ? [$1] : [$1 $3 S $2- $3 A]
def P 2 = 0 $2 $1 A
def X 2 = $2 ? [$1] : [$1 $2 P $2- X]
def F 1 = $1 $1- X
0+++++++!
0 H
0+++ H
0- ? [0+!] : [0-!]
0++++++ 0+++ S!
0++++++ 0+++ M!
0+++ 0++++++ M!
0+ 0++++++++++ M 0++++S!
0++ 0++++ S 0++++ S!
0+++++ 0+++++++ P!
0++++ 0++ M 0+++++++ P!
0++ 0++++ P 0+++++ P!
0+++++F!"""
p.parseAll(p.program, in)