import scala.util.parsing.combinator._

class LogLangParser extends JavaTokenParsers{
    def Task = "task" ~ TaskName ~ ( "{" ~> rep(Operation) <~ "}")
    def TaskName = "[a-zA-Z]*".r
    def Operation = OpName ~ rep(FileName) 
    def FileName = "\"[a-z\\.]*\"".r
    def OpName = "[a-z]*".r
}

val p= new LogLangParser
val inp= """task TaskOne { \n remove "application.debug.old" \n rename "application.debug" "application.debug.old"\n}"""
p.parseAll(p.Task, inp)
src.close
