class Method(m: String) {
    def of(obj: AnyRef) = {
        val f = obj.getClass.getDeclaredMethod(m)
        f.invoke(obj)
    }
}

object Value {
    def of(s: String) = {
        new Method(s)
    }
}

trait Decapsulation {
    val value = Value
}

class Bar {
    def someInternalMethod() = "ciao pep"
}

class BarTest extends Decapsulation {
    val bar = new Bar
    val qux = value of "someInternalMethod" of bar
}