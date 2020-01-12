import scala.util.parsing.combinator._

class DeskParserCombinator extends JavaTokenParsers {
    def sum(l:List[Any], m:Map[String, Int]) : Int = { 
        l match {
            case(h:Int)::tl => h + sum(tl,m)
            case(h:String)::tl => m(h) + sum(tl,m)
            case _ => 0
        }
    }
    def init = "print" ~> expr ~ ("where" ~> variables) ^^ {
        case expr ~ variables => sum(expr, variables.toMap)
    }
    def variables = repsep("[a-z ?]+= ?-?[0-9]+".r, ",") ^^ { items => 
        items.map{ x=> 
            var a = x.split("=")
            (a(0).trim, a(1).trim.toInt)
        }
    }
    def chars = "[a-z]+".r ^^ {_.toString}
    def number = "-?[0-9]+".r ^^ {_.toInt}
    def element = chars | number
    def expr  = repsep(element, "+") ^^ { x => x }
}
import scala.io.Source

object DeskEvaluator {
    def main(args: Array[String]) = {
        val p = new DeskParserCombinator
        args.foreach { filename =>
            val src = scala.io.Source.fromFile(filename)
            val lines = src.mkString
            p.parseAll(p.init, lines) match {
                case p.Success(s,_) => println(s)
                case x => print(x.toString)
            }
            src.close()
        }
    }
}