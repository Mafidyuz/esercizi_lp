import scala.util.parsing.combinator._
import scala.io.Source

class DeskParser extends JavaTokenParsers {
    override val skipWhitespace = false
    def program = "print" ~> "[ ]+".r ~> expr ~ ("where" ~> assignments) ^^ {
        case ls ~ map => ls.foldLeft(0){(x: Int,y: Any) => 
            if (y.isInstanceOf[Char])
                map(y.asInstanceOf[Char]) + x
            else
                x + y.asInstanceOf[Int]
        }
    }
    def assignments = ".*".r ^^ {s => s.split(',').toList.map(_.trim).map(_.split('=').toList.map(_.trim)).map(ls => (ls(0).charAt(0), ls(1).toInt)).toMap} //trasforma una stringa e.g "x = 4, y = 5" in una Map[Char, Int] (x -> 4, y -> 5)
    def expr = rep(single)
    def single = variable <~ ("+" | " ")
    def variable = num|chr
    def chr = "[a-z]".r ^^ {s => s.charAt(0)}
    def num = "-?[0-9]+".r ^^ {_.toInt}
}


object DeskEvaluator {
    def main(args: Array[String]) = {
        val p = new DeskParser
        args.foreach { arg =>
            p.parseAll(p.program, arg) match {
                case p.Success(s,_) => println(s)
                case x => print(x.toString)
            }
        }
    }
}