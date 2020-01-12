import scala.util.parsing.combinator._
import scala.io.Source
import java.util.Scanner

class BrainFuck(cells: Array[Byte]) extends JavaTokenParsers {
    var datapointer = 0
    val in = new Scanner(System.in) 
    def program = statementList ^^ {x => "end"}
    def statementList: Parser[Any] = rep(statement) 
    
    def bodyStatementList: Parser[Unit] = "[" ~> rep(absStatment) <~ "]"^^ {
        case s => while(cells(datapointer) != 0) { parseAll(statementList, s.foldLeft("")(_ + _)) }
    }
    def absStatment = (">" | "<" | "+" | "-" | "." | "," | bodyStatementList | spuriousStr) 

    def statement = (">" | "<" | "+" | "-" | "." | "," | bodyStatementList | spuriousStr) ^^ {
        case ">" => datapointer += 1
        case "<" => datapointer -= 1
        case "+" => cells(datapointer) = (cells(datapointer) + 1).toByte
        case "-" => cells(datapointer) = (cells(datapointer) - 1).toByte
        case "." => println(cells(datapointer).toChar)
        case "," => cells(datapointer) = in.next.charAt(0).toByte
        case _ => 
    }
    def spuriousStr = "[^<>+-\\.,\\[\\]]+".r ^^ {x => ""}
}

at scala.util.parsing.combinator.Parsers$$anon$3.apply(Parsers.scala:222)
at scala.util.parsing.combinator.Parsers$$anon$3.apply(Parsers.scala:222)
at scala.util.parsing.combinator.Parsers$Success.flatMapWithNext(Parsers.scala:143)
at scala.util.parsing.combinator.Parsers$Parser.$anonfun$flatMap$1(Parsers.scala:239)
at scala.util.parsing.combinator.Parsers$$anon$3.apply(Parsers.scala:222)
at scala.util.parsing.combinator.Parsers$Parser.$anonfun
val parser = new BrainFuck(new Array[Byte](30000))

//val input = """[<++[<+<+]<+++<]"""
//parser.parseAll(parser.bodyStatementList, input)

val src = scala.io.Source.fromFile("hw.bf")
parser.parseAll(parser.program, src.mkString)
src.close