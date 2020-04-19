package coroutines

import kotlinx.coroutines.channels.*
import kotlinx.coroutines.*

fun channelBasic() = runBlocking {
    val channel = Channel<Int>()
    launch {
        for ( x in 1..5) channel.send(x * x)
    }
    repeat(5) { print("${channel.receive()} ") }
    println()
    launch {
        for ( x in 1..5) channel.send(x * x)
    }
    channel.close()
    for (y in channel) print("$y ")
    println()
    println("Done!")
}

fun pipelines() = runBlocking {
    fun CoroutineScope.produceSquares(): ReceiveChannel<Int> = produce {
        for (x in 1..5) send(x * x)
    }
    val squares = produceSquares()
    squares.consumeEach { print("$it ") }
    println()
    println("Done")
}

fun primesWithChannel() = runBlocking {
    fun CoroutineScope.numbersFrom(start: Int) = produce<Int> {
        var x = start
        while (true) send(x++) // infinite stream of integers from start
    }

    fun CoroutineScope.filter(numbers: ReceiveChannel<Int>, prime: Int) = produce<Int> {
        for (x in numbers) if (x % prime != 0) send(x)
    }

    var cur = numbersFrom(2)
    repeat(2) {
        val prime = cur.receive()
        println(prime)
        cur = filter(cur, prime)
    }
    coroutineContext.cancelChildren()
}

fun fanout() = runBlocking {
    fun CoroutineScope.produceNumbers() = produce<Int> {
        var x = 1
        while (true) {
            send(x++)
            delay(100)
        }
    }
    fun CoroutineScope.launchProcessor(id: Int, channel: ReceiveChannel<Int>) = launch {
        for (msg in channel) {
            println("Processor #$id received $msg")
        }
    }
    val producer = produceNumbers()
    repeat(5) { launchProcessor(it, producer) }
    delay(100)
    producer.cancel()
}

fun fanin() = runBlocking {
    suspend fun sendString(channel: SendChannel<String>, s: String, time: Long) {
        while (true) {
            delay(time)
            channel.send(s)
        }
    }
    val channel = Channel<String>();
    launch { sendString(channel, "foo", 200L) }
    launch { sendString(channel, "BAR!", 500L) }
    repeat(6) {
        println(channel.receive())
    }
    coroutineContext.cancelChildren()
}

fun bufferedChannel() = runBlocking {
    val channel = Channel<Int>(4)
    val sender = launch {
        repeat(10) {
            println("Sending $it")
            channel.send(it)
        }
    }
    delay(100)
    sender.cancel()
}

data class Ball(var hits: Int)

fun channelsAreFair() = runBlocking {
    suspend fun player(name: String, table: Channel<Ball>) {
        for(ball in table) {
            ball.hits++
            println("$name $ball")
            delay(50)
            table.send(ball)
        }
    }
    val table = Channel<Ball>()
    launch { player("ping", table) }
    launch { player("pong", table) }
    table.send(Ball(0))
    delay(1000)
    coroutineContext.cancelChildren()
}

fun tickerChannels() = runBlocking {
    val tickerChannel = ticker(delayMillis = 100, initialDelayMillis = 0)
    var nextElement = withTimeoutOrNull(1) { tickerChannel.receive() }
    println("Initial element is available immediately: $nextElement")
    nextElement = withTimeoutOrNull(50) { tickerChannel.receive() }
    println("Next element is not ready in 50 ms: $nextElement")

    nextElement = withTimeoutOrNull(60) { tickerChannel.receive() }
    println("Next element is ready in 100 ms: $nextElement")
    println("Consumer pauses for 150ms")
    delay(150)
    nextElement = withTimeoutOrNull(1) { tickerChannel.receive() }
    println("Next element is available immediately after large consumer delay: $nextElement")
    nextElement = withTimeoutOrNull(60) { tickerChannel.receive() }
    println("Next element is ready in 50ms after consumer pause in 150ms: $nextElement")
    tickerChannel.cancel()
}

fun main() {
    channelBasic()
    pipelines()
    primesWithChannel()
    fanout()
    fanin()
    channelsAreFair()
    tickerChannels()
}