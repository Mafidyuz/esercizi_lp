/*
Define a simple calculator DSL that parses (and evaluates) scripts containing several expressions one each line. The admitted operators are: +, -, *, /, ^, sqrt, sin, cos, tan and parentheses with the traditional meanings; all of them can work on integers and reals value. The result for the script evaluation is a printout of the results of the single values.
*/


case class Number(val n: BigDecimal) {
    def + (N: Number) = Number(n + N.n)
//    def * (N: BigDecimal) = new Number(n.multiply(N))
    def / (N: BigDecimal) = new Number(n.toDouble / N)
//    def - (N: BigDecimal) = new Number(n.subtract(N))
//    def ^ (N: BigDecimal) = new Number(math.pow(n, N))
}

object Expr{
    def expr(n: BigDecimal) = Number(n)
}

trait Eval {
    val eval = Expr
}

class ExpressionTest extends Eval {
    var n = eval expr 2 / 3
    def show() = print(n.n)
}