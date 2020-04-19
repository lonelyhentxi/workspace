package classesandobjects

fun eval(expr: SCExpr): Double = when (expr) {
    is SCConst -> expr.number
    is SCSum -> eval(expr.e1) + eval(expr.e2)
    SCNotAsNumber -> Double.NaN
}

fun main() {
    /**
     * used as a enum type of class
     */
    println(eval(SCSum(SCConst(1.0), SCConst(2.0))))
    /**
     * sealed class cannot have non-private element
     * sealed class is abstract and it cannot be Instantiated
     */
}