{
  // # 简单匹配
  val bools = Seq(true, false)
  for (bool <- bools) {
    bool match {
      case true => println("Got heads")
      case false => println("Got tails")
    }
  }
  // equals to
  // val xx = if () 'aaa' else 'bbb'
}; {
  // # match 中的值、变量和类型
  // 使用 unexpected: Any 来匹配任意值，相当于 default 情况, 或者 _
  // 不覆盖所有的类型，会抛出错误
  // 编写 case 子句时，有一些规则和陷阱需要注意。
  // 在被匹配或者提取的值中，编译器指定大写字母开头的为类型名，小写字母开头的为变量名
  // 如果要引用之前的变量可以使用反引号
  def checkY(y: Int) = {
    for {
      x <- Seq(99, 100, 101)
    } {
      val str = x match {
        case `y` => "found y"
        case i: Int => "int: " + i
      }
      println(str)
    }
  }

  checkY(100)
  // 否则会视为一个新的变量名，匹配一切
}; {
  // # 序列的匹配
  def seqToString[T](seq: Seq[T]): String = seq match {
    case Nil => "Nil"
    case head +: tail => s"$head +: " + seqToString(tail)
  }

  val nonEmptySeq = Seq(1, 2, 3, 4, 5)
  val emptySeq = Seq.empty[Int]
  val nonEmptyList = List(1, 2, 3, 4, 5)
  for (seq <- Seq(nonEmptySeq, emptySeq, nonEmptyList)) {
    println(seqToString(seq))
  }
  // +: 向右结合
  // 解结合，提取头部和尾部元素，对空序列使用会抛出异常
  // Nil 可以匹配所有非空序列，对 list 可以使用 ::
}; {
  // # 元组的匹配
  val aTestTuple = ("scala", "spark")
  aTestTuple match {
    case ("scala", _) => println("Found scala")
    case (start, end) => println("other")
  }
}
// case 中也可以使用 guard 语句
{
  // case 类的匹配
  // match-deep.sc
  // 因为是自动机实现，所以可以使用嵌套匹配
  // ## unapply 方法
  // 除了 case 类之外，自定义的类型，如果实现了 unapply 也可以 match
  // unapply 返回 tupleN
  // match-deep.sc
  // 实际上就是 +: 的中缀表达式形式
  // 逆序处理可以使用 :+ 的形式
  // 两参数泛型也可以转化成中缀
  // ## unapplySeq 方法
  // 可以 unapplySeq 可以匹配多个参数的 Seq，便于创建滑动窗口
  def window2[T](seq: Seq[T]): String = seq match {
    case head1 +: head2 +: tail => s"($head1,$head2)," + window2(tail)
    case head +: tail => s"($head,_)," + window2(tail)
    case Nil => "Nil"
  }
  val lst = List(1,2,3)
  println(window2(lst))
  // 滑动窗口如此有用，Seq 甚至提供了两个方法用于创建窗口
  val seq = Seq(1,2,3,4,5)
  val slide2 = seq.sliding(2)
  println(slide2.toList)
  println(slide2.toSeq)
  println(seq.sliding(3,2).toList)
}