package classesandobjects

inline class ICPassword(val value: String)

inline class ICName(val s: String) {
    val length: Int
        get() = s.length
    fun greet() {
        println("hello, $s")
    }
}

interface ICPrintable {
    fun prettyPrint(): String
}

inline class ICName1(val s: String) : ICPrintable {
    override fun prettyPrint(): String = "Let's $s!"
}

interface ICI

inline class ICFoo(val i: Int) : ICI

fun asInline(f: ICFoo) {}
fun <T> asGeneric(x: T) {}
fun asInterface(i: ICI) {}
fun asNullable(i: ICFoo?) {}
fun <T> id(x: T): T = x

fun main() {
    run {
        @Suppress("UNUSED_VARIABLE")
        val securityPassword = ICPassword("Don't try this in production")
    }
    /**
     * inner classes can declare properties and methods
     * but cannot use init block and can not own backing fields
     */
    run {
        val name = ICName("kotlin")
        name.greet()
        println(name.length)
    }
    /**
     * can implement interface but can not derived and be derived
     */
    run {
        val name = ICName1("kotlin")
        println(name.prettyPrint())
    }
    run {
        val f = ICFoo(42)
        asInline(f) // inline
        asGeneric(f) // boxed
        asInline(f) // boxed
        asNullable(f) // boxed
        @Suppress("UNUSED_VARIABLE")
        val c = id(f) // boxed and unbox
    }
    /**
     * Since inline classes are compiled to their underlying type,
     * it may lead to various obscure errors,
     * for example unexpected platform signature clashes
     * To mitigate such issues,
     * functions using inline classes are mangled by adding
     * some stable hashcode to the function name
     */
}