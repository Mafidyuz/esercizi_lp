import scala.util.parsing.combinator._
import scala.io.Source

class CSV(header: List[String], body: List[List[String]]) {
    val nRows = body.length
    val nElements = header.length

    def maxLength(n: Int) = (header::body).map(l => l(n).length).max
    def separators = {
        var l = 0
        for(i<-0 until nElements)
            l+=maxLength(i) + 4
        var s = ""
        for(i<- 0 until l-2)
            s+="-"
        s
    }
    def spaces(n:Int) = {
        var s = ""
        for(i<- 0 until n)
            s+=" "
        s
    }
    def getTable = {
        println(separators)
        for(i <- 0 until nElements)
            print("| %s%s ".format(header(i), spaces(maxLength(i) - header(i).length)))
        println("|")
        println(separators)
        for (row <- body){
            for(i <- 0 until nElements)
                print("| %s%s ".format(row(i), spaces(maxLength(i) - row(i).length)))
            println("|")
        } 
        println(separators)
    }
}

class CSVParser extends JavaTokenParsers {
    def csv = line ~ lines ^^{
        case header ~ body => new CSV(header, body)
    }
    def lines = rep(line) 
    def line = ".+(\n|$)".r ^^ {_.trim.split(',').toList.map(_.trim)}
}

object CSVEvaluator{
    def main(args: Array[String]){
        val parser = new CSVParser
        for(filename <- args){
            val src = scala.io.Source.fromFile(filename)
            parser.parseAll(parser.csv, src.mkString) match {
                case parser.Success(res, _) => res.getTable
                case _ => println("Error")
            }
            src.close
        }
    }
}