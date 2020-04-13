import java.lang.IllegalStateException

fun tryNumbers() {
    val one = 1
    val threeBillion = 300_000_000
    val oneLong = 1L
    val oneByte: Byte = 1
    println("$one $threeBillion $oneLong $oneByte")
    val pi = 3.14
    val pif = 3.14f
    println("$pi $pif")
    val x0f = 0x0f
    val b10 = 0b10
    println("$x0f $b10")
    val a: Int = 10000
    val boxedA: Int? = a
    val anotherBoxedA: Int? = a
    println(boxedA === anotherBoxedA)
    println(boxedA == anotherBoxedA)
    println(a.toLong())
    // support unsigned type since 1.3, via 1u 1uL
}

fun tryOperators() {
    val x = 5L/2;
    println(x==2L)
    val y = (1 shl 2) and 0x000FF000
    println(y)
    /*
        for float, support == != < > <= >= a..b in !in
        support approximate comparable
     */
}

fun tryChar(c: Char): Int {
    if (c !in '0'..'9') {
        throw IllegalStateException("Out of range")
    }
    return c.toInt() - '0'.toInt()
}

fun tryArray() {
    val asc = Array(5) { (it*it).toString() }
    asc.forEach { print("$it ") }
    println()
    val arr = IntArray(10) { it+1 }
    println(arr.joinToString(","))
}

fun tryString() {
    println("tryString")
    val a = """
        try string!
    """
    println(a)
}

fun tryJumpExpr() {
    var sum = 0
    loop@ for (i in 1..100) {
        for (j in 1..100) {
            if (j+i==50) continue@loop
            sum += (j+i)
        }
    }
    println(sum)
    listOf(1,2,3,4,5).forEach lit@ {
        if (it==3) return@lit
        print("$it ")
    }
    println()
    listOf(1,2,3,4,5).forEach {
        if (it==3) return@forEach
        print("$it ")
    }
    println()
    listOf(1,2,3,4,5).forEach (fun(x) {
        if (x == 3) return
        print("$x ")
    })
    println()
    run loop@ {
        listOf(1, 2, 3, 4, 5).forEach {
            if (it == 3) return@loop
            print("$it ")
        }
        println()
    }
}

fun main() {
    tryNumbers()
    tryOperators()
    println(tryChar('4'))
    tryArray()
    tryString()
    tryJumpExpr()
}

