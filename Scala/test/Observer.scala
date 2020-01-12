import scala.language.reflectiveCalls

trait Subject {
    private var observers = List[Observer]()
    type Observer = {def update(subject: Any)}
    def addObserver(o: Observer) = observers::=o
    def removeObserver(o: Observer)
    def notifyObservers() = observers.foreach(_.update(this))
}

