package allcollections

fun operationsIntroduction() {
    /**
     * Member function define operations that are essential for a collection type
     * To make the creation of new implementations easier,
     * use the skeletal implementations of collection interfaces from the standard library:
     * AbstractCollection, AbstractList, AbstractSet, AbstractMap, and their mutable counterparts
     *
     * Other collection operations are declared as extension functions.
     * These are filtering, transformation, ordering,
     * and other collection processing functions.
     */
}

fun sequenceTransformation() {
    run {
        // mapping
        run {
            val numbers = setOf(1, 2, 3)
            println(numbers.map { it * 3 })
            println(numbers.mapIndexed { idx, value -> value * idx })
        }
        run {
            val numbers = setOf(1, 2, 3)
            println(numbers.mapNotNull { if (it == 2) null else it * 3 })
            println(numbers.mapIndexedNotNull { idx, value -> if (idx == 0) null else value * idx })
        }
        run {
            val numbersMap = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key11" to 11)
            println(numbersMap.mapKeys { it.key.toUpperCase() })
            println(numbersMap.mapValues { it.value + it.key.length })
        }
    }
    run {
        // zipping
        run {
            val colors = listOf("red", "brown", "grey")
            val animals = listOf("fox", "bear", "wolf")
            println(colors zip animals)
            println(colors.zip(animals) { color, animal ->
                "The ${animal.capitalize()} is $color"
            })
            println((colors zip animals).unzip())
        }
    }
    run {
        // Associating with
        run {
            val numbers = listOf("one", "two", "three", "four")
            println(numbers.associateWith { it.length })
            println(numbers.associateBy { it.length })
            println(numbers.associate { it.toUpperCase() to it.length })
        }
    }
    run {
        // flatten
        run {
            val numberSets = listOf(setOf(1, 2, 3), setOf(4, 5, 6), setOf(1, 2))
            println(numberSets.flatten())
            println(numberSets.flatMap { listOf(it.size) })
        }
    }
    run {
        // joining
        run {
            val numbers = listOf("one", "two", "three", "four")
            println(numbers.joinToString())
            println(numbers.joinToString(separator = ",", prefix = "{", postfix = "}"))
        }
        run {
            val numbers = (1..100).toList()
            println(numbers.joinToString(limit = 10, truncated = "<...>"))
        }
        run {
            val numbers = listOf("one", "two", "three", "four")
            println(numbers.joinToString { "Element: ${it.toUpperCase()}" })
        }
    }
}

fun sequenceFilter() {
    run {
        // filtering
        run {
            // by predicate
            run {
                val numbers = listOf("one", "two", "three", "four")
                val longerThan3 = numbers.filter { it.length > 3 }
                println(longerThan3)
                println(numbers.filterNot { it.length > 3 })
                println(numbers.filterIndexed { index, s -> index != 0 && s.length < 5 })
                println(numbers.filterIsInstance<String>())
                println(numbers.filterNotNull())

                val numbersMap = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key11" to 11)
                val filteredMap = numbersMap.filter { (key, value) -> key.endsWith("1") && value > 10}
                println(filteredMap)
            }
        }
        run {
            // partitioning
            run {
                val numbers = listOf("one", "two", "three", "four")
                val (match, rest) = numbers.partition { it.length > 3 }
                println(match)
                println(rest)
            }
        }
        run {
            // testing predicates
            run {
                val numbers = listOf("one", "two", "three", "four")

                println(numbers.any { it.endsWith("e") })
                println(numbers.none { it.endsWith("a") })
                println(numbers.all { it.endsWith("e") })

                println(emptyList<Int>().all { it > 5 })
            }
            run {
                val numbers = listOf("one", "two", "three", "four")
                val empty = emptyList<String>()

                println(numbers.any())
                println(empty.any())

                println(numbers.none())
                println(empty.none())
            }
        }
    }
}

fun plusAndMinusOperation() {
    run {
        // return new collection
        val numbers = listOf("one", "two", "three", "four")
        val plusList = numbers + "five"
        val minusList = numbers - listOf("three", "four")
        println(plusList)
        println(minusList)
    }
    run {
        val a = mutableListOf(1,2)
        val b = a
        b-=1
        println("b ${b.toString()}")
        println("b===a ${b===a}")
    }
    run {
        val a = listOf(1,2)
        var b = a
        b-=1
        println("b ${b.toString()}")
        println("b===a ${b===a}")
    }
}

fun sequenceGrouping() {
    run {
        // grouping
        val numbers = listOf("one", "two", "three", "four", "five")

        println(numbers.groupBy { it.first().toUpperCase() })
        println(numbers.groupBy(keySelector = { it.first() }, valueTransform = { it.toUpperCase() }))
    }
    run {
        /**
         * group operations: do something for each group
         * - eachCount()
         * - fold() reduce()
         * - aggregate()
         */
        val numbers = listOf("one", "two", "three", "four", "five", "six")
        println(numbers.groupingBy { it.first() }.eachCount())
    }
}

fun retrievingCollectionParts() {
    run {
        // slice
        val numbers = listOf("one", "two", "three", "four", "five", "six")
        println(numbers.slice(1..3))
        println(numbers.slice(0..4 step 2))
        println(numbers.slice(setOf(3, 5, 0)))
    }
    run {
        // take & drop
        run {
            val numbers = listOf("one", "two", "three", "four", "five", "six")
            println(numbers.take(3))
            println(numbers.takeLast(3))
            println(numbers.drop(1))
            println(numbers.dropLast(5))
        }
        run {
            val numbers = listOf("one", "two", "three", "four", "five", "six")
            println(numbers.takeWhile { !it.startsWith('f') })
            println(numbers.takeLastWhile { it != "three" })
            println(numbers.dropWhile { it.length == 3 })
            println(numbers.dropLastWhile { it.contains('i') })
        }
    }
    run {
        // chunked
        run {
            val numbers = (0..13).toList()
            println(numbers.chunked(3))
        }
        run {
            val numbers = (0..13).toList()
            println(numbers.chunked(3) { it.sum() })
        }
    }
    run {
        // windowed
        run {
            val numbers = listOf("one", "two", "three", "four", "five")
            println(numbers.windowed(3))
        }
        run {
            val numbers = (1..10).toList()
            println(numbers.windowed(3, step = 2, partialWindows = true))
            println(numbers.windowed(3) { it.sum() })
        }
        run {
            val numbers = listOf("one", "two", "three", "four", "five")
            println(numbers.zipWithNext())
            println(numbers.zipWithNext() { s1, s2 -> s1.length > s2.length})
        }
    }
}

fun retrievingSingleElements() {
    run {
        // by position
        run {
            val numbers = linkedSetOf("one", "two", "three", "four", "five")
            println(numbers.elementAt(3))

            val numbersSortedSet = sortedSetOf("one", "two", "three", "four")
            println(numbersSortedSet.elementAt(0))
        }
        run {
            val numbers = listOf("one", "two", "three", "four", "five")
            println(numbers.first())
            println(numbers.last())
        }
        run {
            val numbers = listOf("one", "two", "three", "four", "five")
            println(numbers.elementAtOrNull(5))
            println(numbers.elementAtOrElse(5)
                { index -> "The value for index $index is undefined"})
        }
    }
    run {
        // by condition
        run {
            val numbers = listOf("one", "two", "three", "four", "five", "six")
            println(numbers.first { it.length > 3 })
            println(numbers.last { it.startsWith("f") })
        }
        run {
            val numbers = listOf("one", "two", "three", "four", "five", "six")
            println(numbers.firstOrNull { it.length > 6 })
        }
        run {
            val numbers = listOf(1, 2, 3, 4)
            println(numbers.find { it % 2 == 0 })
            println(numbers.findLast { it % 2 == 0 })
        }
    }
    run {
        // random
        val numbers = listOf(1, 2, 3, 4)
        println(numbers.random())
    }
    run {
        // existance
        run {
            val numbers = listOf("one", "two", "three", "four", "five", "six")
            println(numbers.contains("four"))
            println("zero" in numbers)

            println(numbers.containsAll(listOf("four", "two")))
            println(numbers.containsAll(listOf("one", "zero")))
        }
        run {
            val numbers = listOf("one", "two", "three", "four", "five", "six")
            println(numbers.isEmpty())
            println(numbers.isNotEmpty())

            val empty = emptyList<String>()
            println(empty.isEmpty())
            println(empty.isNotEmpty())
        }
    }
}

class Version(val major: Int, val minor: Int): Comparable<Version> {
    override fun compareTo(other: Version): Int {
        if (this.major != other.major) {
            return this.major - other.major
        } else if (this.minor != other.minor) {
            return this.minor - other.minor
        } else return 0
    }
}

fun collectionOrdering() {
    run {
        // compare
        run {
            println(Version(1, 2) > Version(1, 3))
            println(Version(2, 0) > Version(1, 5))
        }
        run {
            val lengthComparator = Comparator { str1: String, str2: String -> str1.length - str2.length }
            println(listOf("aaa", "bb", "c").sortedWith(lengthComparator))
        }
        run {
            println(listOf("aaa", "bb", "c").sortedWith(compareBy { it.length }))
        }
    }
    run {
        // natural orders
        run {
            val numbers = listOf("one", "two", "three", "four")

            println("Sorted ascending: ${numbers.sorted()}")
            println("Sorted descending: ${numbers.sortedDescending()}")
        }
    }
    run {
        // custom orders
        run {
            val numbers = listOf("one", "two", "three", "four")

            val sortedNumbers = numbers.sortedBy { it.length }
            println("Sorted by length ascending: $sortedNumbers")
            val sortedByLast = numbers.sortedByDescending { it.last() }
            println("Sorted by the last letter descending: $sortedByLast")
        }
        run {
            val numbers = listOf("one", "two", "three", "four")
            println("Sorted by length ascending: ${numbers.sortedWith(compareBy { it.length })}")
        }
    }
    run {
        // reverse order
        run {
            // new instance
            val numbers = listOf("one", "two", "three", "four")
            println(numbers.reversed())
        }
        run {
            // reverse view
            val numbers = listOf("one", "two", "three", "four")
            val reversedNumbers = numbers.asReversed()
            println(reversedNumbers)
        }
        run {
            // mutable view
            val numbers = mutableListOf("one", "two", "three", "four")
            val reversedNumbers = numbers.asReversed()
            println(reversedNumbers)
            numbers.add("five")
            println(reversedNumbers)
        }
    }
    run {
        // shuffled
        run {
            val numbers = listOf("one", "two", "three", "four")
            println(numbers.shuffled())
        }
    }
}

fun collectionAggregateOperations() {
    run {
        // simple operations
        run {
            val numbers = listOf(6, 42, 10, 4)

            println("Count: ${numbers.count()}")
            println("Max: ${numbers.max()}")
            println("Min: ${numbers.min()}")
            println("Average: ${numbers.average()}")
            println("Sum: ${numbers.sum()}")
        }
        run {
            val numbers = listOf(5, 42, 10, 4)
            val min3Remainder = numbers.minBy { it % 3 }
            println(min3Remainder)

            val strings = listOf("one", "two", "three", "four")
            val longestString = strings.maxWith(compareBy { it.length })
            println(longestString)
        }
        run {
            val numbers = listOf(5, 42, 10, 4)
            println(numbers.sumBy { it * 2 })
            println(numbers.sumByDouble { it.toDouble() / 2 })
        }
    }
    run {
        // fold and reduce
        run {
            val numbers = listOf(5, 2, 10, 4)

            val sum = numbers.reduce { sum, element -> sum + element }
            println(sum)
            val sumDoubled = numbers.fold(0) { sum, element -> sum + element * 2 }
            println(sumDoubled)
        }
        run {
            // from right
            val numbers = listOf(5, 2, 10, 4)
            val sumDoubledRight = numbers.foldRight(0) { element, sum -> sum + element * 2 }
            println(sumDoubledRight)
        }
        run {
            // indexed
            val numbers = listOf(5, 2, 10, 4)
            val sumEven = numbers.foldIndexed(0) { idx, sum, element -> if (idx % 2 == 0) sum + element else sum }
            println(sumEven)

            val sumEvenRight = numbers.foldRightIndexed(0) { idx, element, sum -> if (idx % 2 == 0) sum + element else sum }
            println(sumEvenRight)
        }
    }
}

fun collectionWriteOperations() {
    run {
        // add elements
        run {
            val numbers = mutableListOf(1, 2, 3, 4)
            numbers.add(5)
            println(numbers)
        }
        run {
            val numbers = mutableListOf(1, 2, 5, 6)
            numbers.addAll(arrayOf(7, 8))
            println(numbers)
            numbers.addAll(2, setOf(3, 4))
            println(numbers)
        }
        run {
            val numbers = mutableListOf("one", "two")
            numbers += "three"
            println(numbers)
            numbers += listOf("four", "five")
            println(numbers)
        }
    }
    run {
        // remove elements
        run {
            val numbers = mutableListOf(1, 2, 3, 4, 3)
            numbers.remove(3)
            println(numbers)
            numbers.remove(5)
            println(numbers)
        }
        run {
            val numbers = mutableListOf(1, 2, 3, 4)
            println(numbers)
            numbers.retainAll { it >= 3 }
            println(numbers)
            numbers.clear()
            println(numbers)
            val numbersSet = mutableSetOf("one", "two", "three", "four")
            numbersSet.removeAll(setOf("one", "two"))
            println(numbersSet)
        }
        run {
            val numbers = mutableListOf("one", "two", "three", "three", "four")
            numbers -= "three"
            println(numbers)
            numbers -= listOf("four", "five")
            println(numbers)
        }
    }
}

fun main() {
    operationsIntroduction()
    sequenceTransformation()
    sequenceFilter()
    plusAndMinusOperation()
    sequenceGrouping()
    retrievingCollectionParts()
    retrievingSingleElements()
    collectionAggregateOperations()
    collectionWriteOperations()
}