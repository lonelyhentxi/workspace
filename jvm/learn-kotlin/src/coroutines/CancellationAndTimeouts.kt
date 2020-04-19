package coroutines

import kotlinx.coroutines.*

fun tryCancellation() = runBlocking {
    val job = launch {
        repeat(200) {
            i ->
            println("job: I'm sleeping $i ...")
            delay(100L)
        }
    }
    delay(320L)
    println("main: I'm tired of waiting!")
    job.cancel()
    job.join()
    println("main: Now I can quit.")
}

fun cancellationIsCooperative() = runBlocking {
    val startTime = System.currentTimeMillis()
    val job = launch(Dispatchers.Default) {
        var nextPrintTime = startTime
        var i = 0
        while (i<5) {
            if (System.currentTimeMillis() >= nextPrintTime) {
                println("job: I'm sleeping ${i++} ...")
                nextPrintTime += 100L
            }
        }
    }
    delay(260L)
    println("main: I'm tired of waiting!")
    job.cancelAndJoin()
    println("main: Now I can quit.")
}

fun makeCancellable() = runBlocking {
    val startTime = System.currentTimeMillis()
    val job = launch(Dispatchers.Default) {
        var nextPrintTime = startTime
        var i = 0
        while (isActive) {
            if (System.currentTimeMillis() >= nextPrintTime) {
                println("job: I'm sleeping ${i++} ...")
                nextPrintTime += 100L
            }
        }
    }
    delay(260L)
    println("main: I'm tired of waiting!")
    job.cancelAndJoin()
    println("main: Now I can quit.")
}

fun releaseResourceInFinally() = runBlocking {
    val job = launch {
        try {
            repeat(100) { i ->
                println("job: I'm sleeping $i ...")
                delay(100L)
            }
        } finally {
            println("job: I'm running finally")
        }
    }
    delay(260L)
    println("main: I'm tired of waiting!")
    job.cancelAndJoin()
    println("main: Now I can quit.")
}

fun nonCancellableBlock() = runBlocking {
    val job = launch {
        try {
            repeat(1000) { i ->
                println("job: I'm sleeping $i ...")
                delay(100L)
            }
        } finally {
            withContext(NonCancellable) {
                println("job: I'm running finally")
                delay(200L)
                println("job: And I've just delayed for 1 sec because I'm non-cancellable")
            }
        }
    }
    delay(260L)
    println("main: I'm tired of waiting!")
    job.cancelAndJoin()
    println("main: Now I can quit.")
}

fun tryTimeout() = runBlocking {
    try {
        withTimeout(260L) {
            repeat(1000) {
                    i ->
                println("I'm sleeping $i ...")
                delay(100L)
            }
        }
    } catch(e: TimeoutCancellationException) {
        println(e.localizedMessage)
    }
    val result = withTimeoutOrNull(260L) {
        repeat(1000) {
                i ->
            println("I'm sleeping $i ...")
            delay(100L)
        }
        "Done"
    }
    println("Result is $result")
}

fun main() {
    tryCancellation()
    cancellationIsCooperative()
    makeCancellable()
    releaseResourceInFinally()
    nonCancellableBlock()
    tryTimeout()
}