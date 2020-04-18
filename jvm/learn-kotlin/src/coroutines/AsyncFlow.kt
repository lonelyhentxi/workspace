package coroutines

import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlin.system.*

fun basicAsyncSequences(): Sequence<Int> = sequence {
    for (i in 1..3) {
        Thread.sleep(20L)
        yield(i)
    }
}

suspend fun suspendAsyncSequences(): List<Int> {
    delay(200)
    return listOf(1, 2, 3)
}

fun tryAsyncSequences() {
    println("BasicAsyncSeq: ${basicAsyncSequences().joinToString()}")
    runBlocking {
        println("SuspendAsyncSequences: ${suspendAsyncSequences().joinToString()}")
    }
}

fun flowAsyncSequence(): Flow<Int> = flow {
    for (i in 1..3) {
        delay(20L)
        emit(i)
    }
}

fun tryFlowAsync() = runBlocking {
    launch {
        for (k in 1..3) {
            println("I'm not blocked $k")
            delay(20)
        }
    }
    flowAsyncSequence().collect { value -> println("$value ") }
    println()
}

fun codeFlowAsync() = flow {
    println("Flow started")
    for (i in 1..3) {
        delay(20)
        emit(i)
    }
}

fun tryCodeFlowAsync() = runBlocking {
    println("Calling foo...")
    val flow = codeFlowAsync()
    println("Calling collect...")
    flow.collect { value -> print("$value ") }
    println()
    println("Calling collect again...")
    flow.collect { value -> print("$value ") }
    println()
}

fun tryFlowCancellation() = runBlocking {
    val foo: () -> Flow<Int> = {
        flow {
            for (i in 1..3) {
                delay(50)
                println("Emitting $i")
                emit(i)
            }
        }
    }
    withTimeoutOrNull(125) {
        foo().collect { value -> println(value) }
    }
    println("Done")
}

fun flowBuilder() = runBlocking {
    /**
     * - flowOf: build from var args
     * - asFlow: build from a collection
     */
    println((1..3).asFlow().toList().joinToString())
}

suspend fun performRequest(request: Int): String {
    delay(200)
    return "response $request"
}

fun intermediateFlowOperators() = runBlocking {
    run {
        println((1..3).asFlow()
            .map { request -> performRequest(request) }
            .toList().joinToString())
        val numbers = {
            flow {
                try {
                    emit(1)
                    emit(2)
                    println("This line will not execute")
                    emit(3)
                } finally {
                    println("Finally in numbers")
                }
            }
        }
        println(
            numbers()
                .take(2)
                .toList()
                .joinToString()
        )
        val sum = (1..5).asFlow()
            .map { it * it }
            .reduce { a, b -> a + b }
        println(sum)
        println((1..5).asFlow()
            .filter {
                println("Filter $it")
                it % 2 == 0
            }
            .map {
                println("Map $it")
                "string $it"
            }.toList().joinToString()
        )
        // context error
//        val foo = {
//            flow {
//                kotlinx.coroutines.withContext(Dispatchers.Default) {
//                    for (i in 1..3) {
//                        Thread.sleep(100)
//                        emit(i)
//                    }
//                }
//            }
//        }
//        println(foo().toList().joinToString())
        // flowOn
        val bar = {
            flow {
                for (i in 1..3) {
                    Thread.sleep(100)
                    log("Emitting $i")
                    emit(i)
                }
            }.flowOn(Dispatchers.Default)
        }
        println(bar().toList().joinToString())
//    val foo = {
//        flow {
//            for (i in 1..3) {
//                delay(100)
//                emit(i)
//            }
//        }
//    }
//    run {
//        // buffer
//        // just exist on Lkotlin
//        val time = measureTimeMillis {
//            foo().buffer().collect { value ->
//                delay(200)
//                println(value)
//            }
//        }
//        println("Collected in $time ms")
//    }
//    run {
//        // conflate
//        // just exist on lkotlin
//        val time = measureTimeMillis {
//            foo()
//                .conflate()
//                .collect { value ->
//                    delay(300)
//                    println(value)
//                }
//        }
//        println("Collected in $time ms")
//    }
//    run {
//        val time = measureTimeMillis {
//        foo()
//            .collectLatest { value ->
//                println("Collecting $value")
//                delay(300)
//                println("Done $value")
//            }
//        }
//        println("Collected in $time ms")
//    }
    }
    run {
        // combine multiple flows
        run {
            // zip
            val nums = (1..3).asFlow()
            val strs = flowOf("one", "two", "three")
            println(nums.zip(strs) { a, b -> "$a -> $b" }.toList().joinToString())
        }
//        run {
//            // combine
//            // Lkotlin
//            val nums = (1..3).asFlow().onEach { delay(300) } // 发射数字 1..3，间隔 300 毫秒
//            val strs = flowOf("one", "two", "three").onEach { delay(400) } // 每 400 毫秒发射一次字符串
//            val startTime = System.currentTimeMillis() // 记录开始的时间
//            nums.combine(strs) { a, b -> "$a -> $b" } // 使用“combine”组合单个字符串
//                .collect { value -> // 收集并打印
//                    println("$value at ${System.currentTimeMillis() - startTime} ms from start")
//                }
//        }
    }
    run {
        // flatten
        // lkotlin
//        run {
//            // flatMapConcat
//            val startTime = System.currentTimeMillis()
//            (1..3).asFlow().onEach { delay(100) }
//                .flatMapConcat { requestFlow(it) }
//                .collect { value ->
//                    println("$value at ${System.currentTimeMillis() - startTime} ms from start")
//                }
//        }
//        run {
//            val startTime = System.currentTimeMillis() // 记录开始时间
//            (1..3).asFlow().onEach { delay(100) } // 每 100 毫秒发射一个数字
//                .flatMapMerge { requestFlow(it) }
//                .collect { value -> // 收集并打印
//                    println("$value at ${System.currentTimeMillis() - startTime} ms from start")
//                }
//        }
//        run {
//            val startTime = System.currentTimeMillis() // 记录开始时间
//            (1..3).asFlow().onEach { delay(100) } // 每 100 毫秒发射一个数字
//                .flatMapLatest { requestFlow(it) }
//                .collect { value -> // 收集并打印
//                    println("$value at ${System.currentTimeMillis() - startTime} ms from start")
//                }
//        }
    }
}

fun flowException() = runBlocking {
    val foo = {
        flow {
            for( i in 1..3) {
                println("Emitting $i")
                emit(i)
            }
        }
    }
    try {
        foo()
            .collect {
                    value ->
                println("$value ")
                check(value <=1) { "Collected $value" }
            }
    } catch (e: Throwable) {
        println("Caught $e")
    }
}

//fun flowCatch() = runBlocking {
//    val foo = {
//        flow {
//            for( i in 1..3) {
//                println("Emitting $i")
//                emit(i)
//            }
//        }
//    }
//    foo()
//        .catch { e -> emit("Caught $e") }
//        .collect { value -> println(value) }
//}

fun flowCompletion() = runBlocking {
    fun foo(): Flow<Int> = (1..3).asFlow()
    run {
        try {
            foo().collect { value -> println(value) }
        } finally {
            println("Done")
        }
    }
//    run {
//        foo().onCompletion { println("Done") }
//            .collect { value -> println(value) }
//    }
//    run {
//        // Upstream exceptions only
//        foo()
//            .onCompletion { cause -> println("Flow completed with $cause") }
//            .collect { value ->
//                check(value <= 1) { "Collected $value" }
//                println(value)
//            }
//    }
}

fun main() {
    tryAsyncSequences()
    tryFlowAsync()
    tryCodeFlowAsync()
    tryFlowCancellation()
    flowBuilder()
    intermediateFlowOperators()
    flowException()
}