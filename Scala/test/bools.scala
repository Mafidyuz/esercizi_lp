abstract class Bool {
    def and(b: => Bool): Bool
    def or(b: => Bool): Bool
}

case object True extends Bool{
    def and(b: => Bool) = b
    def or(b: => Bool) = this
}

case object False extends Bool {
    def and(b: => Bool) = this
    def or(b: => Bool) = b
}


