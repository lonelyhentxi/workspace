package functions

import java.lang.Math.*

fun read(b: Array<Byte>, off: Int = 0, len: Int = b.size - off): List<Byte> {
    return b.slice(off until off+len)
}

open class FA {
    open fun foo(i: Int = 10) {
        println("FA foo $i")
    }
}

class FB: FA() {
    override fun foo(i: Int) {
        println("FB foo $i")
    }
}

fun foo(bar: Int = 0, baz: Int) {
    println("foo bar=$bar baz=$baz")
}

fun bar(foo: Int=0, baz: Int = 1, qux: ()-> Unit) {
    println("bar foo=$foo baz=$baz")
    qux()
}

fun <T> asList(vararg ts: T): List<T> {
    val result = ArrayList<T>();
    for (t in ts) {
        result.add(t)
    }
    return result
}

infix fun Int.wtf(y: Int): Int {
    if((this xor y)==0) {
        return this;
    } else {
        return y;
    }
}

fun outer() {
    fun inner() {
        println("inner function")
    }
    inner()
    println("outer function")
}

val eps = 1E-10

tailrec fun findFixPoint(x: Double = 1.0): Double
        = if (abs(x - cos(x)) < eps) x else findFixPoint(cos(x))

fun main() {
    run {
        val arr = arrayOf<Byte>(1,2,3,4,5)
        val lst = read(arr, 0)
        println(lst.toString())
    }
    /**
     * override methods always use the same default parameter as super type
     * you should ignore the default parameter in override method
     */
    run {
        FB().foo()
    }
    /**
     * any parameter after parameters that have default value
     * must be called with its name
     */
    run {
        foo(baz=1)
    }
    /**
     * if the last parameter is lambda, it can be called with name in parentheses
     * or as function block outside the parentheses
     */
    run {
        bar(1) { println("hello") }
        bar ( qux = { println("hello") } )
        bar { println("hello") }
    }
    /**
     * when call java function, you can not use named parameters
     * because java will not always save the name information
     */
    /**
     * you can ignore the Unit return type
     */
    /**
     * if there is only single expression in function body block
     * you can use single expression function
     * the expression will be returned
     */
    run {
        val list = arrayOf(1,2,3)
        val a = asList(0, *list)
        println(a.toString())
    }
    /**
     * infix function should:
     * - must be method or extension
     * - require and must have a single parameter
     * - can not receive vararg or default parameter
     */
    run {
        println(1 wtf 2);
    }
    /**
     * support local functions, methods, generics functions
     * inner functions, extensions, high order functions and lambdas
     */
    run {
        outer()
    }

    run {
        println(findFixPoint(10.0))
    }
}