class ConvertRules(rules: Int => String) {
    def apply(x: Int) = {
        try{ rules(x) }
        catch {
            case th: Throwable => new Exception("Failed convertions asd")
        }
    }
}

object rules {
    def apply(rules: Int => String) = new ConvertRules(rules)
}