package classesandobjects

import java.text.FieldPosition

interface MyInterface {
    val prop: Int

    val propertyWithImplementation: String
        get() = "foo"

    fun bar()
    fun foo() {
        println("foo")
    }
}

class Child : MyInterface {
    override fun bar() {
        println("bar")
    }

    override val prop = 1;
}

interface Named {
    val name: String
}

interface NamedPerson : Named {
    val firstName: String
    val lastName: String
    override val name: String get() = firstName + lastName
}

data class NamedEmployee(
    override val firstName: String,
    override val lastName: String,
    val level: Int
) : NamedPerson

fun main() {
    /**
     * 1. implement the interface function
     * 2. override the property of interface
     */
    run {
        val c = Child()
        c.bar()
        c.foo()
        println(c.propertyWithImplementation)
        println(c.prop)
    }
    /**
     * derive the interface
     */
    run {
        val namedEmployee = NamedEmployee("Jack", "Ma", 0)
        println("${namedEmployee.name}'s level is ${namedEmployee.level}")
    }
    /**
     * when implement multiple interfaces, use super<Interface>.method()/field
     */

}