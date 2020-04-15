package classesandobjects

class NICOuter {
    private val bar: Int = 1
    class Nested {
        fun foo() = 2
    }
    inner class Inner {
        private val bar: Int = 1
        fun foo() = bar + this@NICOuter.bar
    }
}

fun main() {
    run {
        println("nested class: ${NICOuter.Nested().foo()}")
        println("inner class: ${NICOuter().Inner().foo()}")
    }
    /**
     * Anonymous inner class instances are created using an object expression
     */
}