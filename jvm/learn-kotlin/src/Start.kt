

import java.io.File
import java.lang.Exception
import java.lang.IllegalStateException
import java.nio.file.Files
import java.nio.file.Paths
import kotlin.text.*

fun sum(a: Int, b: Int): Int {
    return a + b;
}

fun printSum(a: Int, b: Int): Unit {
    println("${sum(a, b)}")
}

val PI = 3.14;
var d = 0;

fun tryVariables(): Unit {
    val a: Int = 1
    val b = a
    val c: Int
    c = b
    println("c is $c")
    var x = 5;
    x += 1;
    println("x is $x")
    println("PI is $PI")
    d += 2;
    println("d is $d")
}

fun typeCheckAndAutomaticCasts(obj: Any): Int? {
    if (obj is String && obj.length > 0) {
        return obj.length
    }
    if (obj !is Int) {
        return null
    }
    return obj
}

fun mayBeNull(x: Int): Int? {
    return if (x % 2 == 1) x else null
}

fun basicForIn(strs: List<String>): Unit {
    for (str in strs) {
        print("$str ")
    }
    println()
}

fun basicWhile(counter: Int): Int {
    var factor = counter
    var prod = 1
    while (factor > 0) {
        prod *= factor
        factor -= 1
    }
    return prod
}

// single expression function
fun basicWhen(obj: Any): String =
    when (obj) {
        1 -> "One"
        "Hello" -> "Greeting"
        is Long -> "Long"
        !is String -> "Not a string"
        else -> "Unknown"
    }

fun basicRange() {
    val items = mutableListOf<String>()
    for (x in 1..10) {
        items.add(x.toString())
    }
    for (x in 10 downTo 0 step 3) {
        items.add(x.toString())
    }
    println(items.joinToString(", "))
    if (items.size !in 1..100) {
        println("shocked, items size is not in 1-100")
    } else {
        println("surely, items size should in 1-100")
    }
}

fun basicCollections() {
    val fruits = listOf("banana", "avocado", "apple", "kiwifruit")
    fruits
        .filter { it.startsWith("a") }
        .sortedBy { it }
        .map { it.toUpperCase() }
        .forEach { println(it) }
    println("apple is ${if ("apple" in fruits) "in" else "not in"} fruits")
    val map = mutableMapOf("a" to 1, "b" to 2, "c" to 3)
    map["d"] = 4
    println(map)
}

/**
 * DTOs/POJOs/POCOs
 * getters, for mutable variable will have setters
 * equals
 * hashCode
 * toString
 * copy
 * component{1,n}
 */
@Suppress("unused")
data class Customer(val name: String, val email: String)

// default parameters
@Suppress("unused")
fun foo(
    @Suppress("UNUSED_PARAMETER")
    a: Int = 0,
    @Suppress("UNUSED_PARAMETER")
    b: String = "abc"
) {
}

// function blocks
fun funcBlocks() {
    println(listOf("a", "b", "c").filter { x -> x.length <= 1 })
    println(listOf("a", "b", "c").filter { it.length <= 1 })
}

fun tryLazyProperties() {
    val p: String by lazy {
        "abc" + "def"
    }
    println(p)
}

// extension function
fun String.foobar(): String {
    return "foobar"
}

// single instance
@Suppress("unused")
object Resource {
    val name = "Name"
}

fun tryAbbr() {
    val files = File("abbr").listFiles()
    println("files are ${files?.size}")
    println("files or no file? ${files ?: "no file"}")
    try {
        println("files are ${files ?: throw IllegalStateException("file is missing")}")
    } catch (e: Exception) {
        println("should throw exception: ${e.localizedMessage}")
    }
    val value: String? = "foobar"
    println(value?.let { x -> if (x == "foobar") null else x.toString() } ?: "null")
}

class Turtle {
    var a = 1;
    var b = 2;
    fun penDown() = println("penDown")
    fun penUp() = println("penUp")
    fun turn(degrees: Double) = println("turn $degrees degree")
    fun forward(pixels: Double) = println("forward $pixels pixel")
}

fun tryWith() {
    val myTurtle = Turtle().apply {
        a = 1
        b = 1
    }
    with(myTurtle) { // 画一个 100 像素的正方形
        penDown()
        for (i in 1..4) {
            forward(100.0)
            turn(90.0)
        }
        penUp()
        println("turtle's a is $b")
    }
}

fun tryTryWithResource() {
    val stream = Files.newInputStream(Paths.get("learn-kotlin.iml"))
    stream.buffered().reader().use { reader ->
        println(reader.readText().length)
    }
}

fun tryAlso() {
    var a = 1
    var b = 0
    a = b.also { b = a }
    println("a is $a and b is $b")
}

fun tryToDo() {
    try {
        TODO("Waiting for do")
    } catch (e: NotImplementedError) {
        println(e.localizedMessage)
    }
}

fun main() {
    println("Hello world")
    println("${sum(1, 2)}")
    printSum(1, 2)
    tryVariables()
    // single line comment
    /*
    multiline comment
     */
    // see string template in the above
    if (sum(1, 2) > 1) {
        println("sum(1,2)>1")
    } else {
        println("sum(1,2)<=1")
    }
    val ifExprRet = if (sum(1, 2) > 1) "sum(1,2)>1" else "sum(1,2)<=1"
    println(ifExprRet)
    println("mayBeNull(2) is ${mayBeNull(2)}")
    if (mayBeNull(2) == null) {
        println("mayBeNull(2) is null")
    }
    println("int of obj is ${typeCheckAndAutomaticCasts("cde")}")
    basicForIn(listOf("a", "ab", "abc"))
    println("3! is ${basicWhile(3)}")
    println("when long, then ${basicWhen("Hello")}")
    basicRange()
    basicCollections()
    funcBlocks()
    tryLazyProperties()
    println("abc".foobar())
    tryAbbr()
    tryWith()
    tryTryWithResource()
    tryAlso()
    tryToDo()
}

/** styles see [coding-conventions](https://www.kotlincn.net/docs/reference/coding-conventions.html) */
