package allcollections

fun collectionIntroduction() {
    /**
     * A pair of interfaces represent each collection type:
     * - read-only
     * - mutable
     * The read-only collection types are covariant
     */
    /**
     * Collection:
     * - size: Int
     * - in
     * - iterable
     *
     * MutableCollection:
     * - *<Collection>
     * - add(value: T)
     * - remove()
     */
    /**
     * List: equals, default ArrayList
     * - *<Collection>
     * - get(index: Int)/operator[](index: Int)/indexOf(index: Int)
     *
     * MutableList
     * - set(index: Int)/operator[](index: Int, value: T)
     * - removeAt(index: Int)
     * - *<MutableCollection>
     */
    /**
     * Set: equivalent
     * - *<Collection>
     * - contains(key: T)
     *
     * MutableSet, default LinkedHashSet Ordered
     * - *<MutableCollection>
     * - add(key: T)/remove(key: T)
     */
    /**
     * Map: (key, value)::equivalent, default LinkedHashSet Ordered
     * - *<Collection>
     * - contains(key: K)
     * - get(key: K)/operator[](key: K)
     * - keys / values
     *
     * MutableMap:
     * - put(key: K)/operator[](key: K, value: V)
     */
}

fun collectionConstructing() {
    /**
     * Basic constructor with vararg
     * - mutable(Map|Set|List|Array)Of\(vararg\)
     * - (map|set|list|array)Of\(vararg\)
     * For map should use `to` to build Pair objects
     */
    println(mapOf("a" to 1, "b" to 2).toString())
    /**
     * Empty constructor
     * empty(List|Set|Map)\(\)
     * should have type hints
     */
    println(emptyList<Int>().toString())
    /**
     * Concrete type constructor
     * ArrayList/LinkedList/……
     */
    println(HashSet<Int>(32).toString())
    /**
     * Initializer functions for lists
     */
    println((List(3) {it * 2}).toString())
    /**
     * Copy & Ref
     */
    val list  = listOf(1,2,3,4)
    val mutList = list.toMutableList()
    mutList.add(5)
    println("raw list is ${list.toString()}")
}

fun collectionIterators() {
    /**
     * Iterator interface
     * hasNext(): Boolean
     * next(): T
     */
    run {
        val numbers = listOf("one", "two", "three", "four")
        val numbersIterator = numbers.iterator()
        while (numbersIterator.hasNext()) {
            println(numbersIterator.next())
        }
    }
    /**
     * ListIterator
     * hasPrevious(): Boolean
     * previous(): T
     */
    run {
        val numbers = listOf("one", "two", "three", "four")
        val listIterator = numbers.listIterator()
        while (listIterator.hasNext()) listIterator.next()
        println("Iterating backwards:")
        while (listIterator.hasPrevious()) {
            print("Index: ${listIterator.previousIndex()}")
            println(", value: ${listIterator.previous()}")
        }
    }
    /**
     * MutableIterator
     * *<Iterator>
     * remove()
     */
    run {
        val numbers = mutableListOf("one", "two", "three", "four")
        val mutableIterator = numbers.iterator()

        mutableIterator.next()
        mutableIterator.remove()
        println("After removal: $numbers")
    }
    /**
     * MutableListIterator
     * *<ListIterator>
     * *<MutableIterator>
     * add(value: V)
     * set(value: V)
     */
    run {
        val numbers = mutableListOf("one", "four", "four")
        val mutableListIterator = numbers.listIterator()

        mutableListIterator.next()
        mutableListIterator.add("two")
        mutableListIterator.next()
        mutableListIterator.set("three")
        println(numbers)
    }
}

fun main() {
    collectionIntroduction()
    collectionConstructing()
    collectionIterators()
}