package classesandobjects

/**
 * will automatic implement:
 * 1. equals, hashCode
 * 2. toString
 * 3. componentN
 * 4. copy
 * should match the requests:
 * 1. constructor should have at least one parameter
 * 2. all parameters should be marked as val or var
 * 3. data class can not be sealed, abstract, inner or open
 * other laws:
 * 1. if manually implement equals, hashCode and toString or there are final methods in super type,
 *    it will use them rather than automatically generate methods
 * 2. if there are open componentN and return compatible type,
 *    it will generate those methods automatically
 * 3. can not implement componentN or copy manually
 */
@Suppress("UNUSED")
data class DCUser(val name: String, val age: Int)

/**
 * if you want to use non-param data classes, please use default parameter
 */
@Suppress("UNUSED")
data class DCNonParamUser(val name: String = "abc", val age: Int = 1)

/**
 * automatically generated methods will only exist in main constructor
 */
@Suppress("UNUSED")
data class DCPerson(val name: String) {
    var age: Int = 0
}

/**
 * function destruction will only be used for data classes
 */

/**
 * there are pair and tuple in the standard library
 */
