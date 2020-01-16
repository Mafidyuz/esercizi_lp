import scala.util.parsing.combinator

class ExprParser extends JavaTokenParsers{
    def start:Parser[Int] = (expr ~ ("+" | "-") ~ start | expr) ^^ {
        case (a:Int) ~ "+" ~ (b:Int) => a + b
        case (a:Int) ~ "-" ~ (b:Int) => a - b
        case (x:Int) => x
    }
    def expr: Parser[Int] = (fac ~ ("*" | "/") ~ expr | fac) ^^ {
        case (a:Int) ~ "*" ~ (b:Int) => a * b
        case (a:Int) ~ "/" ~ (b:Int) => a / b
        case (x:Int) => x
    }
    def fac = num | start  
    def num = "-?[0-9]+".r ^^ {_.toInt}
    def op = "+"|"-"|"*"|"/"
}


val p = new ExprParser
p.parseAll(p.start, "2*3/2") 