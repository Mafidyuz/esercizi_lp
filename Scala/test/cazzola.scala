import util.parsing.combinator.RegexParsers

trait CSVParser extends RegexParsers {
override val skipWhitespace = false
override val whiteSpace = """[ \t]""".r
var row_sizes: List[Int] = Nil
var tmp_sizes: List[Int] = Nil
var firstTime = true
def file: Parser[String] = hdr ~! rep1(row) ^^ {
case header ~ rows =>
val row_len = (3*row_sizes.length+1+row_sizes./:(0)(_ + _))
val str_format = row_sizes.map(n =>
"| %%-%ds ".format(n)).:\[String]("|\n")(_+_)
"-"*row_len+"\n"+str_format.format(header.toSeq:_*)+"-"*row_len+"\n"+
rows.map(r => str_format.format(r.toSeq:_*)).:\[String]("")(_+_)+
"-"*row_len+"\n"
}
def hdr: Parser[List[String]] = row
def row: Parser[List[String]] = repsep(field, ",") <~ """\r""".? <~ "\n" ^^
{s =>
if (firstTime) {
row_sizes = tmp_sizes
firstTime = false
} else row_sizes = (row_sizes, tmp_sizes).zipped map (_ max _);
tmp_sizes = Nil
s
}
def field: Parser[String] = (TEXT ||| STRING | EMPTY) ^^ {
s => tmp_sizes = tmp_sizes :+ s.length(); s }
lazy val TEXT: Parser[String] = rep1("""[^,\n\r\"]""".r) ^^ makeText
lazy val STRING: Parser[String] =
whiteSpace.* ~> "\"" ~> rep("\"\"" | """[^\"]""".r)
<~ "\"" <~ whiteSpace.* ^^ makeString
lazy val EMPTY: Parser[String] = "" ^^ makeEmpty
def makeText: (List[String]) => String
def makeString: (List[String]) => String
def makeEmpty: String => String
}trait CSVParserAction {
// remove leading and trailing blanks
def makeText = (text: List[String]) => text.mkString("").trim
// remove embracing quotation marks
// replace double quotes by single quotes
def makeString =
(string: List[String]) => string.mkString("").replaceAll("\"\"", "\"")
// modify result of EMPTY token if required
def makeEmpty = (string: String) => ""
}
object CSVParserCLI {
def main(args: Array[String]) {
args.foreach { filename =>
val p = new CSVParser with CSVParserAction
val src = scala.io.Source.fromFile(filename)
val lines = src.mkString
p.parseAll(p.file, lines) match {
case p.Success(t,_) =>
println(t)
case x => print(x.toString)
}
src.close()
}
}
}