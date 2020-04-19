package coroutines

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*
import kotlinx.coroutines.selects.*
import java.util.*

fun selectInChannel() = runBlocking {
    fun CoroutineScope.fizz() = produce<String> {
        while (true) {
            delay(30)
            send("Fizz")
        }
    }
    fun CoroutineScope.buzz() = produce<String> {
        while (true) {
            delay(50)
            send("Buzz!")
        }
    }
    suspend fun selectFizzBuzz(fizz: ReceiveChannel<String>, buzz: ReceiveChannel<String>) {
        select<Unit> {
            fizz.onReceive {
                value ->
                    println("fizz -> '$value'")
            }
            buzz.onReceive {
                value ->
                    println("buzz -> '$value'")
            }
        }
    }
    val fizz = fizz()
    val buzz = buzz()
    repeat(7) {
        selectFizzBuzz(fizz, buzz)
    }
    coroutineContext.cancelChildren()
}

fun selectingOnClose() = runBlocking {
    fun CoroutineScope.fizz() = produce<String> {
        while (true) {
            delay(30)
            send("Fizz")
        }
    }
    fun CoroutineScope.buzz() = produce<String> {
        while (true) {
            delay(50)
            send("Buzz!")
        }
    }
    suspend fun selectAorB(a: ReceiveChannel<String>, b: ReceiveChannel<String>): String =
        select<String> {
            a.onReceiveOrNull { value ->
                if (value == null)
                    "Channel 'a' is closed"
                else
                    "a -> '$value'"
            }
            b.onReceiveOrNull { value ->
                if (value == null)
                    "Channel 'b' is closed"
                else
                    "b -> '$value'"
            }
        }
    val a = produce<String> {
        repeat(4) { send("Hello $it") }
    }
    val b = produce<String> {
        repeat(4) { send("World $it") }
    }
    repeat(8) {
        println(selectAorB(a, b))
    }
    coroutineContext.cancelChildren()
}

fun selectingOnSend() = runBlocking {
    fun CoroutineScope.produceNumbers(side: SendChannel<Int>) = produce<Int> {
        for(num in 1..10) {
            delay(100)
            select<Unit> {
                onSend(num) {}
                side.onSend(num) {}
            }
        }
    }
    val side = Channel<Int>()
    launch {
        side.consumeEach { println("Side channel has $it") }
    }
    produceNumbers(side).consumeEach {
        println("Consuming $it")
        delay(250)
    }
    println("Done consuming")
    coroutineContext.cancelChildren()
}

fun selectingDeferredValues() = runBlocking {
    fun CoroutineScope.asyncString(time: Int) = async {
        delay(time.toLong())
        "Waited for $time ms"
    }
    fun CoroutineScope.asyncStringsList(): List<Deferred<String>> {
        val random = Random(3)
        return List(12) { asyncString(random.nextInt(1000)) }
    }
    val list = asyncStringsList()
    val result = select<String> {
        list.withIndex().forEach { (index, deferred) ->
            deferred.onAwait { answer ->
                "Deferred $index produced answer '$answer'"
            }
        }
    }
    println(result)
    val countActive = list.count { it.isActive }
    println("$countActive coroutines are still active")
}

fun switchOverAChannelOfDeferredValues() = runBlocking {
    fun CoroutineScope.switchMapDeferreds(input: ReceiveChannel<Deferred<String>>) = produce<String> {
        var current = input.receive()
        while (isActive) {
            val next = select<Deferred<String>?> {
                input.onReceiveOrNull { update ->
                    update
                }
                current.onAwait { value ->
                    send(value)
                    input.receiveOrNull()
                }
            }
            if (next == null) {
                println("Channel was closed")
                break
            } else {
                current = next
            }
        }
    }
    fun CoroutineScope.asyncString(str: String, time: Long) = async {
        delay(time)
        str
    }
    val chan = Channel<Deferred<String>>()
    launch {
        for (s in switchMapDeferreds(chan))
            println(s)
    }
    chan.send(asyncString("BEGIN", 100))
    delay(20)
    chan.send(asyncString("Slow", 50))
    delay(10)
    chan.send(asyncString("Replace", 10))
    delay(500)
    chan.send(asyncString("END", 50))
    delay(100)
    chan.close()
    delay(50)
}

fun main() {
    selectInChannel()
    selectingOnClose()
    selectingOnSend()
    selectingDeferredValues()
    switchOverAChannelOfDeferredValues()
}