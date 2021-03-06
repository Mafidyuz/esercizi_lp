import scala.language.postfixOps

/** This example was taken from
  * [[http://www.scala-lang.org/api/current/index.html#scala.util.parsing.combinator.RegexParsers here]]
  * and adapted for the library.
  */
object Calculator extends CharParsers {
  def digit: Parser[Char] = "0123456789"

  def number: Parser[Int] = (digit +) ^^ { _.mkString.toInt }
  def factor: Parser[Int] = number | '(' ~> expr <~ ')'
  def term: Parser[Int] = factor ~ rep('*' ~ factor | '/' ~ factor) ^^ {
    case (number, list) => list.foldLeft(number) {
      case (x, ('*', y)) => x * y
      case (x, ('/', y)) => x / y
    }
  }
  def expr: Parser[Int] = term ~ rep('+' ~ term | '-' ~ term) ^^ {
    case (number, list) => list.foldLeft(number) {
      case (x, ('+', y)) => x + y
      case (x, ('-', y)) => x - y
    }
  }

  def apply(input: String) =
    expr(input) match {
      case Success(result, restInput) => {
        val trimmedInput = restInput.dropWhile(_.isSpaceChar)
        if(trimmedInput.isEmpty)
          println(s"=> $result")
        else
          println(s"Error: End of string expected but `${trimmedInput.head}' found")
      }
      case Failure(message, _) => println(s"Error: $message")
    }
}