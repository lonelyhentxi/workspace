package coroutines

import kotlinx.coroutines.*

fun firstCoroutines() {
    GlobalScope.launch {
        delay(100L)
        println("World!")
    }
    println("Hello,")
    Thread.sleep(200L)
}

fun runBlockingExample() {
    GlobalScope.launch {
        delay(100L)
        println("World!")
    }
    println("Hello,")
    runBlocking {
        delay(200L)
    }
}

fun waitAJob() {
    runBlocking {
        val job = GlobalScope.launch {
            delay(100)
            println("world!")
        }
        println("Hello,")
        job.join()
    }
}

fun structureCoroutines() = runBlocking {
    launch {
        delay(100L)
        println("World!")
    }
    println("Hello,")
}

fun scopeBuilderExample() = runBlocking {
    launch {
        delay(100L)
        println("Task from runBlocking")
    }
    coroutineScope {
        launch {
            delay(200L)
            println("Task from nested launch")
        }
        delay(100L)
        println("Task from coroutine scope")
    }
    println("Coroutine scope is over")
}

suspend fun doWorld() {
    delay(100L)
    println("World!")
}

fun trySuspend() = runBlocking {
    launch {
        doWorld()
    }
    println("Hello,")
}

fun coroutineCosts() = runBlocking {
    repeat(100_000) {
        launch {
            delay(100L)
        }
    }
}

fun main() {
    runBlocking { firstCoroutines() }
    runBlocking { runBlockingExample() }
    waitAJob()
    structureCoroutines()
    scopeBuilderExample()
    trySuspend()
    coroutineCosts()
    GlobalScope.launch {
        repeat(200) {i ->
            println("I'm sleeping $i ...")
            delay(100L)
        }
    }
    runBlocking {
        delay(360L)
    }
}