package progscala.first

object Upper {
  def main(args: Array[String]) = {
    args.map(_.toUpperCase()).foreach(print("%s ",_))
    println()
  }
}