package classesandobjects

/**
 * A sealed class can have subclasses, but all of them
 * must be declared in the same file as the sealed class itself
 */
sealed class SCExpr
data class SCConst(val number: Double) : SCExpr()
data class SCSum(val e1: SCExpr, val e2: SCExpr) : SCExpr()
object SCNotAsNumber : SCExpr()