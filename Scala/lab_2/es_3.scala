import scala.util.parsing.combinator._
import scala.io.Source

class ArithmeticParser extends JavaTokenParsers {

    def resolve(ls: List[(Int, String)]) : Int = ls match {
        case (n,s)::(n1,s1)::tl=> s match {
            case "+" => n + resolve((n1,s1)::tl)
            case "-" => n - 2*n1 + resolve((n1,s1)::tl)
            case "=" => n
        }
        case _ => 0
    }
    def program = rep(row) ^^ {ls => resolve(ls)}
    def row = (number ~ operator | separator) ^^ {
        case (n:Int) ~ "+" => (n,"+")
        case (n:Int) ~ "-" => (n,"-")
        case (n:Int) ~ "=" => (n,"=")
        case _ => (0, "")
    }
    def number = "-?[0-9 ]+".r ^^ {_.trim.toInt}
    def operator = "+"|"-"|"="
    def separator = "[-]+".r
}

object ArithmeticEvaluator {
    def main(args: Array[String]){
        val parser = new ArithmeticParser
        args.foreach{ filename =>
            val src = scala.io.Source.fromFile(filename)
            val input = src.mkString
            parser.parseAll(parser.program, input) match {
                case parser.Success(res, _) => println("%s\n%s".format(input,res))
                case x => println(x)
            }
            src.close()
        }
    }
}