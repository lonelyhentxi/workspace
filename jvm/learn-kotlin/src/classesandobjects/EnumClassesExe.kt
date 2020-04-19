package classesandobjects

inline fun <reified T: Enum<T>> printAllValues() {
    println(enumValues<T>().joinToString { it.name })
}

fun main() {
    run {
        val north = ECDirection.valueOf("NORTH")
        println(north.toString())
        println(ECDirection.values().map { it.toString() }.joinToString(","))
    }
    run {
        printAllValues<ECRGB>()
    }
}