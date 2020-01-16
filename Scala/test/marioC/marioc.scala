// main: rep(statement)
// x = 10
// prt(x | str)
// prtln(x | str)
// +-*/ and or == > <
// if body else body
// if body
// whl
import scala.io.Source
import scala.util.parsing.combinator._
import scala.collection.mutable

case class Var(name: String)

class MarioParser(vars: mutable.Map[String, Any]) extends JavaTokenParsers{
    def main = rep(statement)
    def statement = prt | prtln | declare | operation
    def prt = "prt" ~> "(" ~> (operation | value) <~ ")" ^^ {case x=> print(x)}
    def prtln = "prtln" ~> "(" ~> (operation | value) <~ ")" ^^ {case x=> println(x)}
    def str = stringLiteral ^^ {s => s.substring(1,s.length-1)}
    def varName = """[a-zA-Z]+[0-9]*""".r ^^ {case x => Var(x)}
    def declare = (varName ~ ("=" ~> (operation | value))) ^^ {case s ~ v => vars(s.name) = v}
    def value = str | num | varName ^^ {
        case v: Var => vars(v.name)
        case x => x
    }
    def num = "-?[0-9]+".r ^^ {_.toInt}
    def operation = sum|sub|mul|div
    def sum: Parser[Any] = (value ~ ("+" ~> operation | value)) ^^ {
        case (v1:Int) ~ (v2:Int) => v1+v2
        case (v1:String) ~ (v2:String) => v1+v2
        case x => x
    }
    def sub: Parser[Any] = (value ~ ("-" ~> operation | value)) ^^ {
        case (v1:Int) ~ (v2:Int) => v1-v2
        case x => x
    }
    def mul: Parser[Any] = (value ~ ("*" ~> operation) | value) ^^ {
        case (v1:Int) ~ (v2:Int) => v1*v2
        case x => x
    }
    def div: Parser[Any] = (value ~ ("/" ~> operation) | value) ^^ {
        case (v1:Int) ~ (v2:Int) => v1/v2
        case x => x
    }
    /*
        def operation = value ~ operator ~ (operation | value) ^^{
            case v ~ fun ~ v1 => fun(v,v1)
        }
        def operator = sum | diff | mult | div
        def sum = "+" ^^ {
            case _ => (a,b) => a + b
        }
    */
}

object MarioEval {
    def main(args: Array[String]){
        val p = new MarioParser(mutable.Map[String,Any]())
        val src = scala.io.Source.fromFile(args(0))
        p.parseAll(p.main, src.mkString)
        src.close
    }
}