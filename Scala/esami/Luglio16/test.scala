import scala.util.parsing.combinator._

class WTFParser extends JavaTokenParsers{
    override protected val whiteSpace = """[\ \t]+""".r
    def program = rep(str)
    def str = rep(c) <~ "[\n$]".r
    def c = "ciao"
}

val p = new WTFParser
p.parseAll(p.program, """ciaociao""")