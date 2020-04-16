package functions

inline fun <T> inlineCall(body: () -> T): T {
    return body()
}

inline fun <T> inlineCall(inlineBody: () -> T, noinline noInlineBody: () -> T): T {
    inlineBody()
    return noInlineBody()
}

class TreeNode(val value: Int) {
    var parent: TreeNode? = null
    override fun toString(): String {
        return "{value=${value}}"
    }
}

inline fun <reified T> TreeNode.findParentOfType(): T? {
    var p = parent
    while (p!=null && p !is T) {
        p = p.parent
    }
    return p as T?
}

inline fun <reified T> membersOf() = T::class.members

fun main() {
    /**
     * if want to inline function, please add inline keyword
     * if want to partly inlined, use inline or notInlined keyword on parameter
     */
    run {
        println(inlineCall { 1 })
        println(inlineCall({ 1 }, { 2 }))
    }
    /**
     * in inlined lambda, you can use return keyword
     * and for another context, you should use crossinline keyword
     */
    //    fun foo() {
    //       inlined {
    //            return // OK：该 lambda 表达式是内联的
    //        }
    //     }
    //    inline fun f(crossinline body: () -> Unit) {
    //        val f = object: Runnable {
    //            override fun run() = body()
    //        }
    //        // ……
    //    }
    /**
     * inline functions support reified type parameters
     * so we can use:
     */
    run {
        val t = TreeNode(1)
        t.parent = TreeNode(2)
        println(t.findParentOfType<TreeNode>())
        println(membersOf<StringBuilder>().joinToString(","))
    }
    /**
     * inline modifier can be used on accessor of properties without backing fields
     * or the entire property
     */
    /**
     * public API inline function body can not use non-public declarations
     * an internal declaration can be marked with @publishedAPI
     */
}