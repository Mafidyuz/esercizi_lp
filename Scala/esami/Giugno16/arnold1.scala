import scala.io.Source 
import scala.util.parsing.combinator._
import scala.collection.mutable

case class Variable(name: String)

class ArnoldParser(vars: mutable.Map[String, Int], stack: mutable.Stack[Int]) extends JavaTokenParsers {
    def main = "IT'S" ~> "SHOW" ~>  "TIME" ~> mainBody <~ "YOU" <~ "HAVE" <~ "BEEN" <~ "TERMINATED"
    def mainBody = statements
    def statements: Parser[List[Any]] = rep(print | declare | assignment | loop | cond) 
    def print = "TALK" ~> "TO" ~> "THE" ~> "HAND" ~> (str | variable) ^^{
        case x => println(x)
    }
    def variable = variableName ^^ {case v => vars(v.name)}
    def declare = ("HEY" ~> "CHRISTMAS" ~> "TREE" ~> variableName ~ ("YOU" ~> "SET" ~> "US" ~> "UP" ~> number)) ^^ {
        case variable ~ number => vars(variable.name) = number
    }
    def assignment = "GET" ~> "TO" ~> "THE" ~> "CHOPPER" ~> variableName <~ "HERE" <~ "IS" <~ "MY" <~ "INVITATION" <~ (value ^^ {case v => stack.push(v)}) <~ operations <~ "ENOUGH" <~ "TALK" ^^ {
        case vname => vars(vname.name) = stack.pop 
    }
    def operations = rep(sum|sub|mul|div|eq|greater|or|and)
    def sum = "GET" ~> "UP" ~> value ^^ {
        case x => stack.push(stack.pop + x)
    }
    def sub = "GET" ~> "DOWN" ~> value ^^ {
        case x => stack.push(stack.pop - x)
    }
    def mul = "YOU'RE" ~> "FIRED" ~> value ^^ {
        case x => stack.push(stack.pop * x)
    }
    def div = "HE" ~> "HAD" ~> "TO" ~> "SPLIT" ~> value ^^ {
        case x => stack.push(stack.pop / x)
    }
    def eq = "YOU" ~> "ARE" ~> "NOT" ~> "YOU" ~> "YOU" ~> "ARE" ~> "ME" ~> value ^^ {
        case x => if(x == stack.pop) stack.push(1) else stack.push(0)
    }
    def greater = "LET" ~> "OFF" ~> "SOME" ~> "STEAM" ~> "BENNET" ~> value ^^ {
        case x => if(stack.pop > x) stack.push(1) else stack.push(0)
    }
    def or = "CONSIDER" ~> "THAT" ~> "A" ~> "DIVORCE" ~> value ^^ {
        case x => if((stack.pop | x)!=0) stack.push(1) else stack.push(0)
    }
    def and = "KNOCK" ~> "KNOCK" ~> value ^^ {
        case x => if((stack.pop & x)!=0) stack.push(1) else stack.push(0)
    }
    def cond = ("BECAUSE" ~> "I'M" ~> "GOING" ~> "TO" ~> "SAY" ~> "PLEASE" ~> value ~ ("[" ~> block <~ "]") ~ ("BULLSHIT" ~> "[" ~> block <~ "]" <~ "YOU" <~ "HAVE" <~ "NO" <~ "RESPECT" <~ "FOR" <~ "LOGIC")) ^^{
        case 0 ~ b1 ~ b2 => parseAll(statements, b2)
        case _ ~ b1 ~ b2 => parseAll(statements, b1)
    }
    def loop = "STICK" ~> "AROUND" ~> variableName ~ ("[" ~> block <~ "]") <~ "CHILL" ^^ {
        case v ~ b => while(vars(v.name) != 0) parseAll(statements, b)
    }
    def block = """[^\[\]]+""".r
    def variableName = "[a-zA-Z0-9]+".r ^^ {s => Variable(s)}
    def number = decimalNumber ^^{_.toInt}
    def str = stringLiteral ^^ {s=> s.substring(1, s.length-1)}
    def value = (number|variable)
}

object ArnoldEval {
    def main(args: Array[String]){
        val p = new ArnoldParser(mutable.Map[String, Int](), mutable.Stack[Int]())
        val src = scala.io.Source.fromFile(args(0))
        p.parseAll(p.main, src.mkString)
        src.close
    }
}