@annotation.tailrec
def continue(conditional: => Boolean)(body: => Unit): Unit = {
  if (conditional) {
    body
    continue(conditional)(body)
  }
}

var count = 0
continue(count<5) {
  println(s"at $count")
  count+=1
}