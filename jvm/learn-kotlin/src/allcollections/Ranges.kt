package allcollections

fun main() {
    /**
     * Basic Usage
     */
    run {
        for(i in (1..4)) {
            print("$i ")
        }
        println()
        for(i in 1 until 4) {
            print("$i ")
        }
        println()
        for(i in 4 downTo 1) {
            print("$i ")
        }
        println()
        for(i in 1..4 step 2) {
            print("$i ")
        }
        println()
        /**
         * why not open downTo?????!!!!
         */
    }
}