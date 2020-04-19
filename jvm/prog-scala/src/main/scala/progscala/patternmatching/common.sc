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

  val lst = List(1, 2, 3)
  println(window2(lst))
  // 滑动窗口如此有用，Seq 甚至提供了两个方法用于创建窗口
  val seq = Seq(1, 2, 3, 4, 5)
  val slide2 = seq.sliding(2)
  println(slide2.toList)
  println(slide2.toSeq)
  println(seq.sliding(3, 2).toList)
}; {
  // # 可变参数列表的匹配
  // 见 match-vararglist.sc
}; {
  // # 正则表达式的匹配
  val BookExtractorRE =
    """Book: title=([^,]+),\s+author=(.+)""".r
  val MagazineExtractorRE = """Magazine: title=([^,]+),\s+issue=(.+)""".r

  val catalog = Seq(
    "Book: title=Programming Scala Second Edition, author=Dean Wampler",
    "Magazine: title=The New Yorker, issue=January 2014",
    "Unknown: text=Who put this here??"
  )

  for (item <- catalog) {
    item match {
      case BookExtractorRE(title, author) =>
        println(s"""Book "$title", written by $author """)
      case MagazineExtractorRE(title, issue) =>
        println(s"""Magazine "$title", issue $issue""")
      case entry => println(s"Unrecognized entry: $entry")
    }
  }
  // 普通的三引号不需要转义
  // 插值三引号需要转义
  // scala.util.matching.Regex 定义了若干个用于正则表达式的方法
}; {
  // # 再谈 case 语句的变量绑定
  case class Person(name: String, age: Int)
  val person = Person("mary",10)
  person match {
    case p @Person("mary", age) => println(s"Hi $age gay, $p")
    case _ => println("other")
  }
}; {
  // # 再谈类型匹配
  // JVM 泛型存在类型擦除，无法实现泛型 match
  // 可以使用嵌套匹配的方法
}; {
  // # 封闭继承层级与全覆盖匹配
  // 对封闭基类做模式匹配时，如果匹配了所有分支，那么就是全覆盖的
  // 还可以用一个值代替一个总是返回固定值的表达式
  // 当类型匹配时避免使用枚举类型，编译器无法判断是否完全匹配
}; {
  // # 模式匹配的其他用法
  // 在定义变量以及比较时也可以使用模式匹配
  case class Address(street: String, city: String, country: String)
  case class Person(name: String, age: Int, address: Address)
  val Person(name,age,Address(_,state,_)) = Person("Dean",29,Address(
    "1 Scala Way","CA","USA"))
  println(name)
  println(age)
  println(state)
  val head :+ tail = List(1,2,3)
  println(head)
  println(tail)
  val Seq(a,b,c) = List(1,2,3)
  println(s"$a $b $c")
  // 在 if 中也能使用模式匹配，但是不能使用 _ , 不符合 JVM $eq$eq 规范
}
// 模式匹配匹配可以自定义 unapply 方法控制暴露状态
// 要谨慎的对待默认 case 的情况