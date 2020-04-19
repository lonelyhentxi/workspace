package classesandobjects

@Suppress("unused")
class Invoice {}

@Suppress("unused")
class Empty

@Suppress("unused")
class Person constructor(
    @Suppress("UNUSED_PARAMETER")
    var firstName: String, val lastName: String) {}

@Suppress("unused")
class Family (
    @Suppress("UNUSED_PARAMETER")
    val lastName: String) {}

class InitOrderDemo(name: String) {
    val firstProperty = "First property: $name".also(::println)
    init {
        println("First initializer block that prints ${name}")
    }
    val secondProperty = "Second property: ${name.length}".also(::println)
    init {
        println("Second initializer block that prints ${name.length}")
    }
}

fun tryShowInitOrderDemo() {
    val c = InitOrderDemo("zzr")
    println("${c.firstProperty} ${c.secondProperty}")
}

@Suppress("unused")
class Customer public constructor(
    @Suppress("unused_parameter")
    name: String
) {}

class Human(val name: String) {
    val children: MutableList<Human> = mutableListOf<Human>();
    init {
        println("$name is a human")
    }

    constructor(name: String, parent: Human): this(name) {
        parent.children.add(this)
        println("set ${this.name} as ${parent.name}'s child")
    }
}

fun trySubConstructor() {
    val p = Human("a")
    val human = Human("b", p);
    println(p.children.firstOrNull()?.name)
    println(human.children.size)
}

@Suppress("unused")
class DontCreateMe private constructor () { /*……*/ }

@Suppress("unused")
class WillCreateNonParameterConstructorAutomaticOnJVM(
    @Suppress("unused_parameter")
    val name: String="a") {}

open class Base(a: Int) {
    init {
        println("base is $a")
    }

    open val ctx = 1;

    open fun draw() {
        println("draw")
    }

    fun fill() {
        println("fill $ctx")
    }
}

@Suppress("unused")
class Derived(
    @Suppress("unused_parameter")
    a: Int): Base(a)

class AnotherDerived: Base {
    override val ctx: Int = 4

    constructor(a: Int): super(a) {
        println("another derived is $a")
    }

    constructor(a: Int, b: Int): this(a) {
        println("another derived is $a, $b")
    }

    final override fun draw() {
        println("not draw")
    }

    fun drawSuper() = super.draw()
}

interface Shape {
    val vertexCount: Int;
}

open class Rectangle(override val vertexCount: Int = 4): Shape {
    open fun draw() { println("draw rectangle") }
}
class Polygon: Shape {
    override var vertexCount: Int = 0
}

class FilledRectangle: Rectangle() {
    override fun draw() { println("draw fill rectangle") }
    val borderColor: String get() = "black"

    inner class Filler {
        fun drawAndFill() {
            super@FilledRectangle.draw()
            println("Drawn a filled rectangle with color $borderColor")
        }
    }
}

open class R {
    open fun draw() = println("draw r")
}

interface P {
    fun draw() = println("draw p")
}

class S: R(), P {
    override fun draw() {
        super<R>.draw()
        super<P>.draw()
    }
}

@Suppress("unused")
abstract class AR: R() {
    abstract override fun draw()
}

fun tryDerive() {
    val ad = AnotherDerived(1, 2)
    ad.draw()
    ad.fill()
    ad.drawSuper()
    println(Rectangle().vertexCount)
    val polygon = Polygon()
    polygon.vertexCount+=1;
    println(polygon.vertexCount)
    val fillRectangle = FilledRectangle()
    fillRectangle.Filler().drawAndFill()
    S().draw()
}

fun main() {
    tryShowInitOrderDemo()
    trySubConstructor()
    tryDerive()
}