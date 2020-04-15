package classesandobjects

typealias TAAnySet = Set<Any>
typealias TATSet<T> = Set<T>
typealias TAMyHandler = (Int, String, Any) -> Unit
typealias TAPredicate<T> = (T) -> Boolean

class TA {
    inner class Inner
}

typealias TAInner = TA.Inner

fun foo(p: TAPredicate<Int>) = p(42)

fun main() {
    val f: (Int) -> Boolean = { it > 0 }
    println(foo(f))

    val p: TAPredicate<Int> = { it > 0 }
    println(listOf(1, -2).filter(p))
}