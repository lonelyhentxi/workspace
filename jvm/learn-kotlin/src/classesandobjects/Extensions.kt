package classesandobjects

fun <T> MutableList<T>.swap(index1: Int, index2: Int) {
    val tmp = this[index1]
    this[index1] = this[index2]
    this[index2] = tmp
}

open class ExtShape
class ExtRectangle : ExtShape()

fun ExtShape.getName() = "ExtShape"
fun ExtRectangle.getName() = "ExtRectangle"
fun printClassName(s: ExtShape) {
    println(s.getName())
}

class ExtExample {
    fun printFunctionType() = println("Class method")
}

@Suppress("EXTENSION_SHADOWED_BY_MEMBER")
fun ExtExample.printFunctionType() = println("Extension function")

fun Any?.toString(): String {
    if (this == null) return "null"
    return this.toString()
}

val <T> List<T>.lastIndex: Int
    get() = if (this.isEmpty()) throw IllegalStateException("empty list") else size - 1

class ExtClass {
    companion object {}
}

fun ExtClass.Companion.printCompanion() = println("companion")

class ExtHost(val hostname: String) {
    fun getName() = println("host")
    fun printHostname() = print(hostname)
}

class ExtConnection(val host: ExtHost, val port: Int) {
    fun printPort() = print("port")

    fun getName() = println("connection")
    private fun ExtHost.printConnectionString() {
        printHostname()
        print(":")
        printPort()
        println()
        getName()
        this@ExtConnection.getName()
    }

    fun connect() {
        host.printConnectionString()
    }
}

open class ExtBase {}
class ExtDerived: ExtBase() {}

open class ExtBaseCaller {
    open fun ExtBase.printFunctionInfo() {
        println("Base extension function in ExtBaseCaller")
    }

    open fun ExtDerived.printFunctionInfo() {
        println("Derived extension function in ExtBaseCaller")
    }

    fun call(b: ExtBase) {
        b.printFunctionInfo()
    }
}

class ExtDerivedCaller: ExtBaseCaller() {
    override fun ExtBase.printFunctionInfo() {
        println("Base extension function in ExtDerivedCaller")
    }

    override fun ExtDerived.printFunctionInfo() {
        println("Derived extension function in ExtDerivedCaller")
    }
}

fun main() {
    run {
        val list = mutableListOf(1, 2, 3)
        list.swap(0, 2)
        println(list.joinToString(" "))
    }
    /**
     * extensions is analysed at compiled time
     */
    run {
        printClassName(ExtRectangle())
    }
    /**
     * it will invoke method if there are method and extension at same time
     */
    run {
        ExtExample().printFunctionType()
    }
    /**
     * nullable receiver
     */
    run {
        println((null as Any?).toString())
    }
    /**
     * extensible descriptors, can not set default initializer and there is no private field
     */
    run {
        val list = listOf(1, 2, 3, 4, 5)
        println("last index is ${list.lastIndex}")
    }
    /**
     * companion object extensions
     */
    run {
        ExtClass.Companion.printCompanion()
    }
    /**
     * if you want to use extensions from other packages, just import them
     * and extension receiver will override the dispatch receiver
     */
    run {
        ExtConnection(ExtHost("127.0.0.1"), 80).connect()
    }
    /**
     * extension is virtual for dispatch receiver
     * extension is static for extension receiver
     */
    run {
        ExtBaseCaller().call(ExtBase())
        ExtDerivedCaller().call(ExtBase())
        ExtDerivedCaller().call(ExtDerived())
    }
}