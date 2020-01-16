import scala.util.parsing.combinator._
import scala.io.Source

class ColumnOperationsParser extends JavaTokenParsers {
    override val whiteSpace = """[\t\ ]+""".r
    def program = rep(row) <~ "[-]+".r ^^ {
        case ls => {
            val numbers = ls.unzip._1.iterator
            val operators = ls.unzip._2.iterator
            var res:Int = numbers.next
            while(numbers.hasNext){
                if(operators.next == '+') 
                    res += numbers.next
                else 
                    res -= numbers.next
            }
            res
        }
    }
    def row = num ~ op <~ "\n" ^^ {
        case n ~ op => (n,op)
    }
    def num = "-?[0-9]+".r ^^ {_.toInt}
    def op =( "+"|"-"| "=") ^^ {_.charAt(0)}
}


class WrongArithmeticExpressionException extends Exception 

object ArnoldEval {
    def main(args: Array[String]){
        val p = new ColumnOperationsParser()
        val src = scala.io.Source.fromFile(args(0))
        val input = src.mkString
        p.parseAll(p.program, input) match {
            case p.Success(res, _) => println("%s\n%s".format(input, res))
            case x => println(x)
        }
        src.close
    }
}