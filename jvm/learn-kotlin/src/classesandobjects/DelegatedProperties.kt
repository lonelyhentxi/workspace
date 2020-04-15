package classesandobjects
import kotlin.reflect.KProperty
import kotlin.properties.Delegates
import kotlin.properties.ReadOnlyProperty
import kotlin.properties.ReadWriteProperty

class DPDelegate {
    operator fun getValue(thisRef: Any?, property: KProperty<*>): String {
        return "$thisRef, thank you for delegating '${property.name}' to me!"
    }
    operator fun setValue(thisRef: Any?, property: KProperty<*>, value: String) {
        println("$value has been assigned to '${property.name}' in $thisRef")
    }
}

class DPExample {
    var p: String by DPDelegate()
}

class DPUser {
    var name: String by Delegates.observable("<no name>") {
        _, old, new ->
        println("$old -> $new")
    }
}

class DPMapUser(val map: Map<String, Any?>) {
    val name: String by map
    val age: Int     by map
}

class DPMutableUser(val map: MutableMap<String, Any?>) {
    var name: String by map
    var age: Int     by map
}

/**
class DPC {
    var prop: Type by DPDelegate()
}

class DPC1 {
    private val prop$delegate = DPDelegate()
    var prop: Type
        get() = prop$delegate.getValue(this, this::prop)
        set(value: Type) = prop$delegate.setValue(this, this::prop, value)
}

class ResourceDelegate<T> : ReadOnlyProperty<MyUI, T> {
    override fun getValue(thisRef: MyUI, property: KProperty<*>): T {
    }
}

class ResourceLoader<T>(id: ResourceID<T>) {
    operator fun provideDelegate(
        thisRef: MyUI,
        prop: KProperty<*>
    ): ReadOnlyProperty<MyUI, T> {
        checkProperty(thisRef, prop.name)
        return ResourceDelegate()
    }

    private fun checkProperty(thisRef: MyUI, name: String) { }
}

class MyUI {
    fun <T> bindResource(id: ResourceID<T>): ResourceLoader<T> {}

    val image by bindResource(ResourceID.image_id)
    val text by bindResource(ResourceID.text_id)
}
 */

fun main() {
    /**
     * There are certain common kinds of properties, such as:
     * - lazy properties: the value gets computed only upon first access
     * - observable properties: listeners get notified about changes to the property
     * - storing properties in a map, instead of a separate field for each property
     */
    run {
        val e = DPExample()
        println(e.p)
        e.p = "NEW"
    }
    /**
     * lazy
     * by default, the evaluation of lazy properties is synchronized:
     * the value is computed only in one thread, and all threads will see
     * the same value:
     * - if do not need synchronization, pass LazyThreadSafetyMode.PUBLICATION
     * - if always used in the same thread, pass LazyThreadSafetyMode.NONE
     */
    run {
        val lazyValue: String by lazy {
            println("computed!")
            "Hello"
        }
        println(lazyValue)
        println(lazyValue)
    }
    /**
     * observable
     * vetoable can be used when need intercept
     */
    run {
        val user = DPUser()
        user.name = "first"
        user.name = "second"
    }
    /**
     * store map
     */
    run {
        val user = DPMapUser(mapOf(
            "name" to "John Doe",
            "age" to 25
        ))
        val mutUser = DPMutableUser(mutableMapOf(
            "name" to user.name,
            "age" to user.age
        ))
        println(user.name)
        println(user.age)
        println(mutUser.name)
        println(mutUser.age)
    }
    /**
     * for a read-only property(val), a delegate has to provide an
     * operator function getValue() with the following parameters:
     * - thisRef  - is sametype or supertype of target type
     * - property - must be of type KProperty<*> or its supertype
     * can impl ReadOnlyProperty<int R, out T>
     */
    /**
     * for a mutable property, a delegate has to additionally provide
     * an operator function setValue() with the following parameters:
     * - thisRef - <ignore>
     * - property - <ignore>
     * - value - newValue, same type or super type
     * can impl ReadWriteProperty<in R, T>
     */
}