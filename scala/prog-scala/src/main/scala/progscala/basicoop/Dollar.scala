package progscala.basicoop

class Dollar(val value: Float) extends AnyVal {

  override def toString = "$%.2f".format(value)
}
