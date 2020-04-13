package progscala.basicoop

object RichInt {
  implicit class RichInt(val self: Int) extends AnyVal {
    def toHexString1: String = java.lang.Integer.toHexString(self)
  }
}
