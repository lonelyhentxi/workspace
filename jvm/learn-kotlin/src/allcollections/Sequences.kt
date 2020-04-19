package allcollections

fun main() {
    /**
     * sequence do all operations for each elem
     * iterator do each operation for all elems
     */
    /**
     * from var args
     */
    run {
        val numbersSequence = sequenceOf("four", "three", "two", "one")
        println(numbersSequence.toString())
    }
    /**
     * from collections
     */
    run {
        val numbers = listOf("one", "two", "three", "four")
        val numbersSequence = numbers.asSequence()
        println(numbersSequence.toString())
    }
    /**
     * from function
     */
    run {
        val oddNumbers = generateSequence(1) { it + 2 }
        println(oddNumbers.take(5).toList().toString())
        val oddNumbersLessThan10 = generateSequence(1) { if (it < 10) it + 2 else null }
        println(oddNumbersLessThan10.count())
    }
    /**
     * from chunks
     */
    run {
        val oddNumbers = sequence {
            yield(1)
            yieldAll(listOf(3, 5))
            yieldAll(generateSequence(7) { it + 2 })
        }
        println(oddNumbers.take(5).toList())
    }
    /**
     * - stateless or const number state: map/filter/take/drop
     * - state: linear or more state
     * if returns another sequence that is produced lazily, it's called intermediate
     * or called terminal operations
     */
}