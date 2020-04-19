package classesandobjects

import kotlin.reflect.typeOf

open class OA(x: Int) {
    public open val y: Int = x
}

interface OB {}

class OC {
    private fun foo() = object {
        val x: String = "x"
    }
    fun publicFoo() = object {
        val x: String = "x"
    }
    fun bar() {
        // private function will return anonymous type
        val x1 = foo().x
        println(x1)
        // public function will return Any
    }
}

/**
 * single instance mode
 * can derive super type and implement interface
 */
@Suppress("UNUSED")
object ODataProviderManager {
    val x: Int = 1
}

class OMyClass {
    init {
        println("produce a OMyClass instance")
    }
    companion object Factory {
        fun create(): OMyClass = OMyClass()
    }
}

@Suppress("UNUSED")
class OMyClassWithoutName {
    companion object { }
}

interface OFactory<T> {
    fun create(): T
}

class OMyClass1 {
    init {
        println("factory produce a OMyClass1 instance")
    }
    companion object : OFactory<OMyClass1> {
        override fun create(): OMyClass1 = OMyClass1()
    }
}

fun main() {
    run {
        var ab: OA = object : OA(1), OB {
            override val y = 15
        }
        println(ab.y)
    }
    run {
        OC().bar()
    }
    run {
        OMyClass.create()
    }
    run {
        @Suppress("UNUSED_VARIABLE")
        val f: OFactory<OMyClass1> = OMyClass1
    }
}