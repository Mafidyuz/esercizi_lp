//set field “foo” of bar to “baz”

trait Decapsulation {
    val set = Set
}

class To(name:String, obj: AnyRef) {
    def to(value: Any) = {
        val f = obj.getClass.getDeclaredField(name)
        f.setAccessible(true)
        f.set(obj, value)
    }
}

class Of(val name: String) {
    def of(o: AnyRef): To = new To(name, o)
}

object Set {
  def field(name: String): Of = new Of(name)
}

class Bar(s: String){
    var foo = s
}

class BarTest extends Decapsulation{
    var bar = new Bar("ciao") 

    def test() {
        set field "foo" of bar to "baz"
    }
}

