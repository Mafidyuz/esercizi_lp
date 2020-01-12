import scala.util.parsing.combinator._
import scala.io.Source
import scala.collection.mutable.Stack

class InvalidProgram extends RuntimeException

case class Definitions(defs: scala.collection.immutable.Map[Char, Definition])
case class Definition(nArgs: Int, body: String)

trait Exec
case class Num(n: Int) extends Exec
case class Arg(n: Int, offset: Int) extends Exec
case class Print() extends Exec
case class Function(name: Char) extends Exec
case class Condition(condition: Int, block1: String, block2: String) extends Exec
case class Main(instructions: List[Exec]) extends Exec

case class Program(defs: Definitions, p_main: Main) {
    var stack = new Stack[Int]()
    var args = new Array[Int](10)
    
    def execute(main: Main): Unit = main.instructions.foreach{ 
        exec => exec match {
            case p: Print => println(stack.pop)
            case num: Num => stack.push(num.n)
            case c: Condition => {
                val p = new WTFParser(args)
                var blockToEval = ""
                if(c.condition != 0) 
                    blockToEval = c.block2
                else
                    blockToEval = c.block1
                p.parseAll(p.main, blockToEval) match {
                    case p.Success((res: Main), _) => execute(res)
                    case _ => throw new InvalidProgram
                }              
            }
            case f: Function => {
                val defi = defs.defs(f.name)
                var internalArgs = List[Int]()
                for(i<- 0 until defi.nArgs)
                    internalArgs = stack.pop :: internalArgs
                args = internalArgs.toArray
                val p = new WTFParser(args)
                p.parseAll(p.main, defs.defs(f.name).body) match {
                    case p.Success((res: Main), _) => execute(res)
                    case other => throw new InvalidProgram
                }              
            }
        }
    }

    def run = execute(p_main)
}

class WTFParser(args: Array[Int]) extends JavaTokenParsers {
    def program = definitions ~ main ^^ {
        case d ~ m => Program(d,m)
    }
    def definitions = rep(definition) ^^ {l => Definitions(l.toMap)}
    def definition = "def" ~> fName ~ nArgs ~ ("=" ~> defBody) ^^ {
        case f ~ n ~ b => (f, Definition(n,b))
    }
    def fName = "[A-Z]".r ^^ {_.charAt(0)}
    def nArgs = "[0-9]".r ^^ {_.toInt}
    def defBody = ".*".r

    def main = rep(condition | call) ^^ {
        case l: List[Exec] => Main(l)
    }
    def call = (arg | num | fName | print) ^^ {
        case n: Int => Num(n)
        case '!' => Print()
        case f: Char => Function(f)
    }
    def condition = (num | arg) ~ ("?" ~> "[" ~> block <~ "]" <~ ":" <~ "[") ~ block <~ "]" ^^ {
        case n ~ b1 ~ b2 => Condition(n,b1,b2)
    }
    def block = """[^\]\[]+""".r
    def arg = "\\$[0-9][+-]*".r ^^ {s=> args(s.diff("$-+").toInt - 1) + s.count(_=='+') - s.count(_=='-')}
    def num = "0[+-]*".r ^^ {s=> s.count(_=='+') - s.count(_=='-')}
    def print = "!" ^^ {_.charAt(0)}
}


object WTFEvaluator {
    def main(args: Array[String]){
        val p = new WTFParser(new Array[Int](10))
        val src = scala.io.Source.fromFile(args(0))
        p.parseAll(p.program, src.mkString) match {
            case p.Success(res, _) => res.run
            case x => throw new InvalidProgram
        }
        src.close
    }
}