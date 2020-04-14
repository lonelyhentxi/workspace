package classesandobjects

open class Outer {
    private val a = 1
    protected open val b = 2
    internal val c = 3
    val d = 4
    protected class Nested {
        public val e: Int = 5
    }
}

@Suppress("unused")
class Subclass: Outer() {
    // can not see a
    // can see b c d
    // can see nested and e
    override val b = 5
}

@Suppress("unused")
class Unrelated(
    @Suppress("unused_parameter")
    o: Outer
) {
    // can not see o.a and o.b
    // can see o.c and o.d
    // can not see Outer.Nested and Nested::e
}

fun main() {
    /**
     * for packages
     * 1. default public
     * 2. if private, visible in this file
     * 3. if internal, visible in this module
     * 4. protected is not for the top level declaration
     */
    /**
     * for classes and interfaces
     * 1. if private, visible in this class
     * 2. if protected, visible in this class and its subclass
     * 3. if internal, visible in this module
     * 4. if public, visible for anything, default
     * 5. outer class can not access the private fields of inner class
     */
    /**
     * for constructor
     * 1. default public
     * 2. if internal, visible in public
     * 3. if private, visible in this class
     */

}