package classesandobjects

import java.util.function.BinaryOperator
import java.util.function.IntBinaryOperator

/**
 * basic usage
 */
enum class ECDirection {
    NORTH, SOUTH, WEST, EAST
}

/**
 * initialization
 */
@Suppress("UNUSED")
enum class ECColor(val rgb: Int) {
    RED(0xFF0000),
    GREEN(0x00FF00),
    BLUE(0x0000FF)
}

/***
 * anonymous classes
 */
@Suppress("UNUSED")
enum class ECProtocolState {
    WAITING {
        override fun signal() = TALKING
    },
    TALKING {
        override fun signal() = WAITING
    };

    abstract fun signal(): ECProtocolState
}

/**
 * implementing interfaces in enum classes
 */
@Suppress("UNUSED")
enum class ECIntArithmetrics : BinaryOperator<Int>, IntBinaryOperator {
    PLUS {
        override fun apply(t: Int, u: Int): Int = t + u
    },
    TIMES {
        override fun apply(t: Int, u: Int): Int = t * u
    };
    override fun applyAsInt(t: Int, u: Int) = apply(t, u)
}

enum class ECRGB { RED, GREEN, BLUE }