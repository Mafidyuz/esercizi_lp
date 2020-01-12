import scala.io.Source
import scala.util.parsing.combinator._
import scala.collection.mutable.Stack

class ArnoldCParser(variables: scala.collection.mutable.Map[String, Int], stack: Stack[Int]) extends JavaTokenParsers {
    def main = "IT'S SHOWTIME" ~> mainBody <~ "YOU HAVE BEEN TERMINATED" ^^ {x => (variables, stack)}
    def mainBody: Parser[Any] = rep(statements | conditionalStatement | loop) ^^ {x=>x}
    def statements = print | assignment | declare 
    def strs = "\"" ~> "[\\w ]*".r <~ "\""
    def print = "TALK TO THE HAND" ~> (strs | variable) ^^ {x => println(x)}
    def declare = "HEY CHRISTMAS TREE" ~> variableName ~ ("YOU SET US UP" ~> number) ^^ {
        case s ~ i => variables(s) = i
    }
    def variableName = "[a-zA-Z0-1]+".r 
    def variable = (number | variableName) ^^ {
        case n: Int => n
        case v: String => variables(v)
    }
    def number = "-?[0-9]+".r ^^ {_.toInt}
    def assignment = "GET TO THE CHOPPER" ~> variableName ~ ("HERE IS MY INVITATION" ~> variable ^^ {case v => stack.push(v)}) ~ rep(operations) <~ "ENOUGH TALK" ^^ {
        case varName ~ firstVar ~ operations => variables(varName) = stack.pop
    }
    def operations = arithOperations | logicalOperations
    def arithOperations = sum | sub | mul | div
    def sum = "GET UP" ~> variable ^^ {
        case n => stack.push(stack.pop + n)
    }
    def sub = "GET DOWN" ~> variable ^^ {
        case n => stack.push(stack.pop - n)
    }
    def mul = "YOU'RE FIRED" ~> variable ^^ {
        case n => stack.push(stack.pop * n)
    }
    def div = "HE HAD TO SPLIT" ~> variable ^^ {
        case n => stack.push(stack.pop / n)
    }
    def get = "GET" ~> variableName ^^ { //per debuggare, da togliere alla fine
        case v => variables(v)
    }
    def logicalOperations = equalz | greater | or | and
    def equalz = "YOU ARE NOT YOU YOU ARE ME" ~> variable ^^ {
        case n => stack.pop == n match {
            case true => stack.push(1)
            case _ => stack.push(0)
        }
    }
    def greater = "LET OFF SOME STEAM BENNET" ~> variable ^^ {
        case n => stack.pop > n match {
            case true => stack.push(1)
            case _ => stack.push(0)
        }
    }
    def or = "CONSIDER THAT A DIVORCE" ~> variable ^^ {
        case n => stack.push(stack.pop | n)
    }
    def and = "KNOCK KNOCK" ~> variable ^^ {
        case n => stack.push(stack.pop & n)
    }
    def conditionalStatement = "BECAUSE I'M GOING TO SAY PLEASE" ~> (variableName ^^ {v => variables(v)}) ~ conditionalBlock ~ ("BULLSHIT" ~> conditionalBlock) <~ "YOU HAVE NO RESPECT FOR LOGIC" ^^ {
        case 0 ~ b1 ~ b2 => parseAll(statements, b2)
        case _ ~ b1 ~ b2 => parseAll(statements, b1)
    }
    def conditionalBlock = """(?s)\[.*?\]""".r ^^ {s => s.substring(1,s.length-1)}
    def loop = "STICK AROUND" ~> variableName ~ conditionalBlock <~ "CHILL" ^^ {
        case v ~ b => 
            while(variables(v) != 0)
                parseAll(mainBody, b)
    }
    
}

object ArnoldEvaluator {
    def main(args: Array[String]){
        val parser = new ArnoldCParser(scala.collection.mutable.Map[String, Int](), Stack[Int]())
        for(filename <- args){
            val src = scala.io.Source.fromFile(filename)
            parser.parseAll(parser.main, src.mkString) match {
                case parser.Success(res,_) => println(res)
                case x => println(x)
            }
            src.close
        }
    }
}