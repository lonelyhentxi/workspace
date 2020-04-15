package classesandobjects

class Box<T>(t: T) {
    var value = t
}

/**
 * declaration-site variance
 * covariance with out keyword
 * contravariance with in keyword
 */
interface GSource<out T> {
    fun nextT(): T;
}

@Suppress("UNUSED")
fun demo(strs: GSource<String>) {
    @Suppress("UNUSED_VARIABLE")
    val a: GSource<Any> = strs;
}

interface GComparable<in T> {
    operator fun compareTo(other: T): Int
}

@Suppress("UNUSED")
fun anotherDemo(x: GComparable<Number>) {
    x.compareTo(1.0)
    @Suppress("UNUSED_VARIABLE")
    val a: GComparable<Double> = x
}

fun copyOut(from: Array<out Any>, to: Array<Any>) {
    assert(from.size==to.size)
    for(i in from.indices) {
        to[i] = from[i]
    }
}

fun copyIn(from: Array<Int>, to: Array<in Int>) {
    assert(from.size==to.size)
    for(i in from.indices) {
        to[i] = from[i]
    }
}

@Suppress("UNUSED")
fun <T> gSort(
    @Suppress("UNUSED_PARAMETER")
    list: List<T>,
    @Suppress("UNUSED_PARAMETER")
    threshold: T): List<String>
    where T: CharSequence,
          T: Comparable<T> {
    return list.filter { it > threshold }.map { it.toString() }
}

fun main() {
    run {
        val box = Box<Int>(1)
        println("box value is ${box.value}")
    }
    /**
     * type projection
     */
    run {
        val array1 = arrayOf(1,2,3,4,5)
        val array2: Array<Any> = arrayOf(0,0,0,0,0)
        val array3: Array<Any> = arrayOf(0,0,0,0,0)
        copyIn(array1, array2)
        copyOut(array1, array3)
        println(array2.map { it.toString() }.joinToString(","))
        println(array3.map { it.toString() }.joinToString(","))
    }
    /**
     * star projection
     * 1. Function<*, String> equals to Function<in Nothing, String>
     * 2. Function<Int, *> equals to Function<Int, out Any?>
     * 3. Function<*, *> equals to Function<in Nothing, out Any?>
     */
    /**
     * because of JVM, there is only compile time generics
     */
}