import scala.io.Source
import scala.util.parsing.combinator._
import scala.collection.mutable.Stack
import scala.collection.mutable.Map

class WTFParser(defs: scala.collection.mutable.Map[Char, (Int, String)], stack: scala.collection.mutable.Stack[Int]) extends JavaTokenParsers {
    def definitions = rep(definition) 
    def definition = "def" ~> fName ~ nArgs ~ ("=" ~> body) ^^ {
        case n ~ a ~ b => defs(n) = (a,b)
    }
    def fName = "[A-Z!]".r ^^ {_.charAt(0)}
    def nArgs = "[0-9]".r ^^ {_.toInt}
    def body = ".*".r
    def getMap = defs
    
    def defCall = rep(num | arg) ~ fName ^^ {

    }
    def num = "0[+-]*".r ^^ {s => stack push(s.count(c => c=='+') - s.count(c=> c=='-'))}
    def arg = "\\$[0-9][-+]*".r ^^ {s=> (s.substring(1,2).toInt-1, (s.count(c=>c=='+') - s.count(c=>c=='-')))}
    def call = rep(num) ~> fName ^^ {
        case '!' => println(stack.pop)
        case f => {
            val nArgs = defs(f)._1
            val body = defs(f)._2
            var args = List[Int]()
            for(i<-0 to nArgs)
                args = stack.pop :: args
            
        }
    }
    //def main = 

}

val p = new WTFParser(scala.collection.mutable.Map[Char,(Int,String)](), scala.collection.mutable.Stack[Int]())
//val input = """def H 1 = $1 !
//def S 2 = $2 ? [$1] : [$1+$2-S]
//def M 2 = $2 ? [$1] : [$1-$2-M]
//def A 3 = $2 ? [$1] : [$1 $3 S $2- $3 A]"""
p.parseAll(p.arg, "$2-")