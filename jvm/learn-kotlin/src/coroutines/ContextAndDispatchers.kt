package coroutines

import kotlinx.coroutines.*

@ExperimentalCoroutinesApi
@ObsoleteCoroutinesApi
fun dispatchersAndThreads() = runBlocking {
    launch {
        println("main runBlocking      : I'm working in thread ${Thread.currentThread().name}")
    }
    launch(Dispatchers.Unconfined) {
        println("Unconfined            : I'm working in thread ${Thread.currentThread().name}")
    }
    launch(Dispatchers.Default) {
        println("Default               : I'm working in thread ${Thread.currentThread().name}")
    }
    newSingleThreadContext("MyOwnThread").use {
        launch(it) {
            println("newSingleThreadContext: I'm working in thread ${Thread.currentThread().name}")
        }
    }
}

@ExperimentalCoroutinesApi
fun unconfinedAndConfinedDispatcher() = runBlocking {
    launch(Dispatchers.Unconfined) {
        println("Unconfined      : I'm working in thread ${Thread.currentThread().name}")
        delay(100)
        println("Unconfined      : After delay in thread ${Thread.currentThread().name}")
    }
    launch {
        println("main runBlocking: I'm working in thread ${Thread.currentThread().name}")
        delay(200)
        println("main runBlocking: After delay in thread ${Thread.currentThread().name}")
    }
}

fun log(msg: String) = println("[${Thread.currentThread().name}] $msg")

fun debuggingCoroutineAndThread() = runBlocking {
    val a = async {
        log("I'm computing a piece of the answer")
        6
    }
    val b = async {
        log("I'm computing another piece of the answer")
        7
    }
    log("The answer is ${a.await() * b.await()}")
}

@ObsoleteCoroutinesApi
fun jumpingInDifferentThread() = runBlocking {
    newSingleThreadContext("Ctx1").use { ctx1 ->
        newSingleThreadContext("Ctx2").use {ctx2 ->
            runBlocking(ctx1) {
                log("Started in ctx1")
                withContext(ctx2) {
                    log("Working in ctx2")
                }
                log("Back to ctx1")
            }
        }
    }
}

@ObsoleteCoroutinesApi
fun joinInTheContext() = runBlocking {
    newSingleThreadContext("Ctx3").use {ctx3 ->
        runBlocking(ctx3) {
            println("My job is ${coroutineContext[ctx3.key]}")
            println("My job is active? ${coroutineContext[ctx3.key]?.isActive}")
        }
    }
}

fun childrenOfACoroutine() = runBlocking {
    val request = launch {
        GlobalScope.launch {
            println("job1: I run in GlobalScope and execute independently!")
            delay(200)
            println("job1: I am not affected by cancellation of the request")
        }
        launch {
            delay(20)
            println("job2: I am a child of the request coroutine")
            delay(200)
            println("job2: I will not execute this line if my parent request is cancellation")
        }
    }
    delay(100)
    request.cancel()
    delay(200)
    println("main: Who has survived request cancellation?")
}

fun parentalResponsibilities() = runBlocking {
    val request = launch {
        repeat(3) {i->
            launch {
                delay((i+1) * 40L)
                println("Coroutine $i is done")
            }
        }
        println("request: I'm done and I don't explicitly join my children that are still active")
    }
    request.join()
    println("Now processing of the request is complete")
}

fun namingCoroutinesForDebugging()  = runBlocking {
    log("Started main coroutine")
    val v1 = async(CoroutineName("v1coroutine")) {
        delay(100)
        log("Computing v1")
        252
    }
    val v2 = async(CoroutineName("v2coroutine")) {
        delay(200)
        log("Computing v2")
        6
    }
    log("The answer for v1 / v2 = ${v1.await() / v2.await()}")
}

fun combiningContextElements() = runBlocking {
    launch(Dispatchers.Default + CoroutineName("test")) {
        println("I'm working in thread ${Thread.currentThread().name}")
    }
}

@ObsoleteCoroutinesApi
@ExperimentalCoroutinesApi
fun main() {
    dispatchersAndThreads()
    unconfinedAndConfinedDispatcher()
    debuggingCoroutineAndThread()
    jumpingInDifferentThread()
    joinInTheContext()
    childrenOfACoroutine()
    parentalResponsibilities()
    namingCoroutinesForDebugging()
    combiningContextElements()
}