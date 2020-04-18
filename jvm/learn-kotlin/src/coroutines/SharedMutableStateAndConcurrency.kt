package coroutines

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*
import kotlinx.coroutines.sync.*
import java.util.concurrent.atomic.AtomicInteger
import kotlin.system.*

suspend fun massiveRun(action: suspend () -> Unit) {
    val n = 100  // number of coroutines to launch
    val k = 1000 // times an action is repeated by each coroutine
    val time = measureTimeMillis {
        coroutineScope { // scope for coroutines
            repeat(n) {
                launch {
                    repeat(k) { action() }
                }
            }
        }
    }
    println("Completed ${n * k} actions in $time ms")
}

fun threadSafeDataStructures() = runBlocking {
    val counter = AtomicInteger()
    withContext(Dispatchers.Default) {
        massiveRun {
            counter.incrementAndGet()
        }
    }
    println("Counter = $counter")
}

fun threadConfinementFineGrained() = runBlocking {
    val counterContext = newSingleThreadContext("CounterContext")
    var counter = 0
    withContext(Dispatchers.Default) {
        massiveRun {
            withContext(counterContext) {
                counter ++
            }
        }
    }
    println("Counter = $counter")
}

fun threadConfinementCoarseGrained() = runBlocking {
    val counterContext = newSingleThreadContext("CounterContext")
    var counter = 0
    withContext(counterContext) {
        massiveRun {
            counter++
        }
    }
    println("Counter = $counter")
}

fun mutualExclusion() = runBlocking {
    val mutex = Mutex()
    var counter = 0
    withContext(Dispatchers.Default) {
        massiveRun {
            mutex.withLock {
                counter ++
            }
        }
    }
    println("Counter = $counter")
}

sealed class CounterMsg
object IncCounter: CounterMsg()
class GetCounter(val response: CompletableDeferred<Int>): CounterMsg()

fun CoroutineScope.counterActor() = actor<CounterMsg> {
    var counter = 0
    for (msg in channel) {
        when(msg) {
            is IncCounter -> counter++
            is GetCounter -> msg.response.complete(counter)
        }
    }
}

fun tryActors() = runBlocking {
    val counter = counterActor()
    withContext(Dispatchers.Default) {
        massiveRun {
            counter.send(IncCounter)
        }
    }
    val response = CompletableDeferred<Int>()
    counter.send(GetCounter(response))
    println("Counter = ${response.await()}")
    counter.close()
}

fun main() {
    threadSafeDataStructures()
    threadConfinementFineGrained()
    threadConfinementCoarseGrained()
    mutualExclusion()
    tryActors()
}