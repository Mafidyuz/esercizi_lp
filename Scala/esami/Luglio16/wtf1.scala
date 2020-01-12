import scala.io.Source
import scala.util.parsing.combinator._
import scala.collection.mutable.Stack

class Function(NARGS: Int, FUN: (List[Int] => Unit)){
    val nargs = NARGS
    val fun = FUN
    def eval(ls: List[Int]) = fun(ls)
}

class WTFParser extends JavaTokenParsers {
    var functions : Map[Char, Function] = Map()
    var argsStack : Stack[Int] = Stack()
    def func = "def" ~> funcName ~ nargs ~ ("=" ~> body) ^^ {
        case name ~ nargs ~ body => functions = functions + (name -> (nargs, body))
    }
    def nargs = decimalNumber ^^ {_.toInt}
    def body = nestedCall
    def argument = "\\$[1-9]".r 
    def num = "0[+]*".r ^^ {s=> argsStack.push(s.count(c => c=='+'))} 
    def call = rep(num) ~ funcName ^^ {
        case args ~ name => 
            val nargs = functions(name).nargs
            var internalArgs: List[Int] = List()   
            for(i<- 0 until nargs)    
                internalArgs = argsStack.pop :: internalArgs
            b(1)
            argsStack.push(1)
    }
    def nestedCall = rep(call)
    def funcName = "[A-Z]".r ^^ {s => s.head}
    def print = ""
}

val p = new WTFParser
val input = """def H 2 = $1 !"""
p.parseAll(p.func, input) 


/*
main = rep(riga)
riga = nu
call = repsep(num," ") ~ func
func =  
*/