package functions

fun <T,R> Collection<T>.myFold(
    initial: R,
    combine: (acc: R, nextElem: T) -> R
): R {
    var accumulator: R = initial
    for (element: T in this) {
        accumulator = combine(accumulator, element)
    }
    return accumulator
}

class LIntTransformer: (Int) -> Int {
    override operator fun invoke(x: Int): Int = x + 1
}

fun main() {
    run {
        val items = listOf(1,2,3,4,5)
        val prod = items.myFold(1, Int::times)
        println("production of ${items.toString()} is $prod")
    }
    /**
     * Instantiating a function type
     * - using a code block within a function literal, this can be used as a value:
     *  - a lambda expression { a,b -> a+b }
     *  - an anonymous function fun(s: String): Int { return s.toIntOrNull() ?: 0 }
     * - using a callable reference to an existing declaration:
     *  - a top-level, local, member, or extension function ::isOdd, String::toInt
     *  - a top-level, member, extension property List<Int>::size
     *  - a constructor ::Regex
     * - using instances of a custom class that implements a function type as an interface
     */
    run {
        val intFunction = LIntTransformer()
        println("int transform result is ${intFunction(1)}")
        val a  = { i: Int -> i+1 }
        println("a(1) result is ${a(1)}")
        val repeatFun: String.(Int) -> String = {
            times -> this.repeat(times)
        }
        val twoParameters: (String, Int) -> String = repeatFun
        println(twoParameters("a", 1))
    }
    /**
     * use invoke can call function
     */
    run {
        val c: (Int, Int) -> Int = Int::plus
        println(c.invoke(1,2))
    }
    /**
     * for lambda
     * if it is the last parameter, can use lambda block outside parentheses
     * if there isn't any other parameter, can ignore the parentheses
     * if the lambda have only one parameter, can ignore parameter and ->, only use it
     * lambda will auto return the last value or return with tag
     * when a parameter of lambda is useless, can use _ to replace it
     */
    run {
        val ints = listOf(1,2,3,4)
        ints.filter {
            val shouldFilter = it >= 3
            return@filter shouldFilter
        }
    }
    /**
     * closure
     */
    run {
        var sum = 0
        val ints = listOf(1,2,3,4)
        ints.filter { it > 0 }.forEach {
            sum += it
        }
        println(sum)
    }
    /**
     * function literals with receiver
     */
    run {
        val sum: Int.(Int) -> Int = { other -> plus(other) };
        println(sum(1,2))
        val prod = fun Int.(other: Int): Int = this * other
        println(prod(1,2))
    }
}