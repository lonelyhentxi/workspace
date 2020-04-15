package classesandobjects

interface DBase {
    val message: String
    fun print()
    fun println()
    fun printMessage()
}

class DBaseImpl(val x: Int): DBase {
    override val message = "BaseImpl: x = $x"
    override fun print() {
        print(x)
    }
    override fun println() {
        println(x)
    }

    override fun printMessage() {
        println(message)
    }
}

class DDerived(b: DBase): DBase by b {
    override val message = "Message of Derived"
    override fun println() {
        println("abc")
    }
}

fun main() {
    /**
     * like prototype
     */
    run {
        val b = DBaseImpl(10)
        DDerived(b).print()
        DDerived(b).println()
        DDerived(b).printMessage()
    }
}