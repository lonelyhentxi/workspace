package allcollections

fun listOperations() {
    run {
        // retrieve by index
        val numbers = listOf(1, 2, 3, 4)
        println(numbers.get(0))
        println(numbers[0])
        println(numbers.getOrNull(5))
        println(numbers.getOrElse(5, {it}))
    }
    run {
        // retrieve by slice
        val numbers = (0..13).toList()
        println(numbers.subList(3, 6))
    }
    run {
        // linear search element position
        run {
            val numbers = listOf(1, 2, 3, 4, 2, 5)
            println(numbers.indexOf(2))
            println(numbers.lastIndexOf(2))
        }
        run {
            val numbers = mutableListOf(1, 2, 3, 4)
            println(numbers.indexOfFirst { it > 2})
            println(numbers.indexOfLast { it % 2 == 1})
        }
    }
    run {
        // binary search
        run {
            val numbers = mutableListOf("one", "two", "three", "four")
            numbers.sort()
            println(numbers)
            println(numbers.binarySearch("two"))  // 3
            println(numbers.binarySearch("z")) // -5
            println(numbers.binarySearch("two", 0, 2))  // -3
        }
        // more see as https://www.kotlincn.net/docs/reference/list-operations.html
    }
    run {
        // list write
        run {
            // add
            val numbers = mutableListOf("one", "five", "six")
            numbers.add(1, "two")
            numbers.addAll(2, listOf("three", "four"))
            println(numbers)
        }
        run {
            // update
            run {
                val numbers = mutableListOf("one", "five", "three")
                numbers[1] =  "two"
                println(numbers)
            }
            run {
                val numbers = mutableListOf(1, 2, 3, 4)
                numbers.fill(3)
                println(numbers)
            }
        }
        run {
            // delete
            val numbers = mutableListOf(1, 2, 3, 4, 3)
            numbers.removeAt(1)
            println(numbers)
        }
    }
    run {
        // sort
        val numbers = mutableListOf("one", "two", "three", "four")

        numbers.sort()
        println("Sort into ascending: $numbers")
        numbers.sortDescending()
        println("Sort into descending: $numbers")

        numbers.sortBy { it.length }
        println("Sort into ascending by length: $numbers")
        numbers.sortByDescending { it.last() }
        println("Sort into descending by the last letter: $numbers")

        numbers.sortWith(compareBy<String> { it.length }.thenBy { it })
        println("Sort by Comparator: $numbers")

        numbers.shuffle()
        println("Shuffle: $numbers")

        numbers.reverse()
        println("Reverse: $numbers")
    }
}

fun setOperations() {
    run {
        val numbers = setOf("one", "two", "three")

        println(numbers union setOf("four", "five"))
        println(setOf("four", "five") union numbers)

        println(numbers intersect setOf("two", "one"))
        println(numbers subtract setOf("three", "four"))
        println(numbers subtract setOf("four", "three"))
    }
}

fun mapOperations() {
    run {
        // retrieve key and value
        run {
            val numbersMap = mapOf("one" to 1, "two" to 2, "three" to 3)
            println(numbersMap.get("one"))
            println(numbersMap["one"])
            println(numbersMap.getOrDefault("four", 10))
            println(numbersMap["five"])
        }
        run {
            val numbersMap = mapOf("one" to 1, "two" to 2, "three" to 3)
            println(numbersMap.keys)
            println(numbersMap.values)
        }
    }
    run {
        // filter
        run {
            val numbersMap = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key11" to 11)
            val filteredMap = numbersMap.filter { (key, value) -> key.endsWith("1") && value > 10}
            println(filteredMap)
        }
        run {
            val numbersMap = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key11" to 11)
            val filteredKeysMap = numbersMap.filterKeys { it.endsWith("1") }
            val filteredValuesMap = numbersMap.filterValues { it < 10 }

            println(filteredKeysMap)
            println(filteredValuesMap)
        }
    }
    run {
        // plus and minus
        run {
            val numbersMap = mapOf("one" to 1, "two" to 2, "three" to 3)
            println(numbersMap + Pair("four", 4))
            println(numbersMap + Pair("one", 10))
            println(numbersMap + mapOf("five" to 5, "one" to 11))
        }
        run {
            val numbersMap = mapOf("one" to 1, "two" to 2, "three" to 3)
            println(numbersMap - "one")
            println(numbersMap - listOf("two", "four"))
        }
    }
    run {
        // map write operations
        run {
            // add adn update
            run {
                val numbersMap = mutableMapOf("one" to 1, "two" to 2)
                numbersMap.put("three", 3)
                println(numbersMap)
            }
            run {
                val numbersMap = mutableMapOf("one" to 1, "two" to 2, "three" to 3)
                numbersMap.putAll(setOf("four" to 4, "five" to 5))
                println(numbersMap)
            }
            run {
                val numbersMap = mutableMapOf("one" to 1, "two" to 2)
                val previousValue = numbersMap.put("one", 11)
                println("value associated with 'one', before: $previousValue, after: ${numbersMap["one"]}")
                println(numbersMap)
            }
            run {
                val numbersMap = mutableMapOf("one" to 1, "two" to 2)
                numbersMap["three"] = 3     // 调用 numbersMap.put("three", 3)
                numbersMap += mapOf("four" to 4, "five" to 5)
                println(numbersMap)
            }
        }
        run {
            // delete
            run {
                val numbersMap = mutableMapOf("one" to 1, "two" to 2, "three" to 3)
                numbersMap.remove("one")
                println(numbersMap)
                numbersMap.remove("three", 4)
                println(numbersMap)
            }
            run {
                val numbersMap = mutableMapOf("one" to 1, "two" to 2, "three" to 3, "threeAgain" to 3)
                numbersMap.keys.remove("one")
                println(numbersMap)
                numbersMap.values.remove(3)
                println(numbersMap)
            }
            run {
                val numbersMap = mutableMapOf("one" to 1, "two" to 2, "three" to 3)
                numbersMap -= "two"
                println(numbersMap)
                numbersMap -= "five"
                println(numbersMap)
            }
        }
    }
}

fun main() {
    listOperations()
    setOperations()
    mapOperations()
}