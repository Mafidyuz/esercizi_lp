//print x+y+z+1+x+-3 where x = 25, y = 1, z=-7   res = 42

import scala.util.parsing.combinator._

class ExprParser extends JavaTokenParsers{

    def program = "print" ~> expr ~ ("where" ~> declarations) 
    def expr = "[a-z\\+\\-0-9]*".r  
    def declarations = ".*".r ^^ {_.split(',').map(_.split('=').toList.map(_.trim)).toList.map(ls => (ls(0).charAt(0), ls(1).toInt)).toMap}
}

val parser = new ExprParser
parser.parseAll(parser.program, "print x+y+z+1+x+-3 where x = 25, y = 1, z=-7")