class editor(s: String, c: Int){
    var txt = s
    var cursor = c

    def x() = {
        txt = txt.substring(0,cursor) + txt.substring(cursor+1, txt.length)
        if (cursor >= txt.length - 1) 
            cursor-=1
    }

    def dw() = {
        var hd = txt.substring(0,cursor)
        var tl = txt.substring(cursor+1, txt.length)
        var i = tl.indexOf(' ')
        if (i == -1)
            txt = hd
        else
            txt =  hd + tl.substring(i, tl.length)
        if(cursor >= txt.length - 1)
            cursor = 0
    }

    def i(c: Char) = txt = txt.substring(0,cursor) + c + txt.substring(cursor, txt.length)

    def iw(w: String) = txt = txt.substring(0,cursor+1) + w + " " + txt.substring(cursor+1, txt.length)

    def l(i: Int = 1) = cursor = if(cursor + i > txt.length) txt.length else cursor + i

    def h(i: Int = 1) = cursor = if(cursor - i < 0) 0 else cursor - i  

    def prints() = print(txt)
}

trait debug extends editor {
    abstract override def prints() = { 
        print(System.getProperty("user.name") + "> ") 
        super.prints()
        print("\nCursor: " + cursor)
    }
}


import scala.collection.mutable.Stack

trait UndoRedo extends editor {
    private var undo = Stack[String]()
    private var redo = Stack[String]()

    override def x() = {
        undo.push(txt)
        super.x
        redo = Stack[String]()
    }

    override def dw() = {
        undo.push(txt)
        super.dw
        redo = Stack[String]()
    }

    override def i(c:Char) = {
        undo.push(txt)
        super.i(c)
        redo = Stack[String]()
    }

    override def iw(w:String) = {
        undo.push(txt)
        super.iw(w)
        redo = Stack[String]()
    }

    def u = {
        if(!undo.isEmpty){
            redo.push(txt)
            txt = undo.pop
        }
    }

    def r = {
        if(!redo.isEmpty) {
            undo.push(txt)
            txt = redo.pop
        }     
    }

}