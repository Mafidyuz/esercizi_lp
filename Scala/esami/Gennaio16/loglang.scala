import scala.util.parsing.combinator._
import scala.io.Source
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter


class LogLangParser extends JavaTokenParsers {
    def program = rep(task) ^^ {ls => 
        var output = ""
        for(s<-ls)
            output+=s
        output
    }
    def task = "task" ~> taskName ~ taskBody ^^ {
        case name ~ ls => {
            var output = "Task %s\n".format(name) 
            for(i <- 1 to ls.length)
                output+="\t[op%s] %s\n".format(i,ls(i-1))
            output
        }
    }
    def taskName = "[a-zA-Z]*".r
    def taskBody = "{" ~> rep(operation) <~ "}"
    def operation = remove | rename | merge | backup
    def filename = stringLiteral ^^ {s => s.substring(1,s.length-1)}
    def remove = "remove" ~> filename ^^ {
        case file => 
            val f = new File(file)
            f.delete
    }
    def rename = "rename" ~> filename ~ filename ^^ {
        case oldName ~ newName =>
            val oldFile = new File(oldName)
            val newFile = new File(newName)
            oldFile.renameTo(newFile)
    }
    def merge = "merge" ~> filename ~ filename ~ filename ^^ {
        case f1 ~ f2 ~ newFile => {
            try{
                val file1 = new File(f1)
                val file2 = new File(f2)
                val src1 = scala.io.Source.fromFile(f1)
                val src2 = scala.io.Source.fromFile(f2)
                if (file1.exists && file2.exists){
                    val writer = new BufferedWriter(new FileWriter(new File(newFile)))
                    var input = src1.mkString + "\n" + src2.mkString
                    writer.write(input)
                    writer.close
                    src1.close
                    src2.close
                    true
                }
                else false
            } catch {
                case _ : Throwable => false
            }
        }
    }
    def backup = "backup" ~> filename ~ filename ^^ {
        case file ~ backupName => 
            val f = new File(file)
            if (f.exists){
                val writer = new BufferedWriter(new FileWriter(new File(backupName)))
                val src = scala.io.Source.fromFile(file)
                writer.write(src.mkString)
                writer.close
                src.close
                true
            }
            else false
    }
}

object LogLangEvaluator {
    def main(args: Array[String]){
        val filename = args(0)
        val src = scala.io.Source.fromFile(filename)
        val parser = new LogLangParser
        parser.parseAll(parser.program, src.mkString) match {
            case parser.Success(res,_)  => println(res)
            case _ => println("Error")
        }
        src.close
    }
}