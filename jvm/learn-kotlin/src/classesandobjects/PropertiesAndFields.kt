package classesandobjects

class Address {
    var lastName: String = "Sherlock"
        private set

    var firstName = "Holmes"
        set(value) {
            if(value!="Holmes") {
                println("you are not sherlock")
            }
            field = value
        }

    val isSherlock: Boolean
        get() = this.lastName == "Sherlock" && this.firstName=="Holmes"
}

fun tryDescriptors() {
    val add = Address()
    add.firstName="?"
    println(add.isSherlock)
}

@Suppress("unused")
const val SUBSYSTEM_DEPRECATED: String = "This subsystem is deprecated"

class MyTest {
    lateinit var subject: Address

    fun setup() {
        subject = Address()
    }

    fun show() {
        if(this::subject.isInitialized) {
            println("is init? ${subject.isSherlock}")
        }
    }
}

fun tryLatentInt() {
    val myTest = MyTest()
    myTest.show()
    myTest.setup()
    myTest.show()
}

fun main() {
    tryDescriptors()
    tryLatentInt()
}