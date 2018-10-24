// scala 函数式编程
{
  // 函数式编程
  // 并发的需求
  // 编写数据导向程序的需求
  // 编写无 bug 程序的需求
}; {
  // # scala 中的函数式编程
  println((1 to 10) filter (_ % 2 == 0) map (_ * 2) reduce (_ * _));
  {
    // ## 匿名函数、lambda 和闭包
    val factor = 10
    val multiplier = (i: Int) => i * factor
    println(multiplier(10))
  }
  {
    // ## 内部与外部的纯粹性
    // 需要保持线程安全和透明引用
  }
}; {
  // # 递归
  // 会带来反复调用函数的开销，栈溢出的风险
}; {
  // # 尾部调用和尾部调用优化
  // scala 可以将尾递归化为循环
  {
    // ## 尾递归和 trampoline 优化
    // 指通过依次调用各个函数完成一系列函数之间的循环
    import scala.util.control.TailCalls._
    def isEven(xs: List[Int]): TailRec[Boolean] =
      if (xs.isEmpty) done(true) else tailcall(isOdd(xs.tail))

    def isOdd(xs: List[Int]): TailRec[Boolean] =
      if (xs.isEmpty) done(false) else tailcall(isEven(xs.tail))

    for (i <- 1 to 5) {
      val even = isEven((1 to i).toList).result
      println(s"$i is even? $even")
    }
  }
}; {
  // # 偏函数和偏应用函数
  // 偏函数指只有部分输入值能返回输出值的情况
  // 偏应用函数是一个表达式，带部分而非全部函数
  // 返回值是一个新函数，负责携带剩下的参数列表
}; {
  // # curry 化和函数的其他转换
  def cat1(s1: String)(s2: String) = s1 + s2

  def cat2(s1: String) = (s2: String) => s1 + s2

  val cat3 = cat2("a+")
  println(cat3("b"))

  def cat4(s1: String, s2: String) = s1 + s2

  val cat4Carried = (cat4 _).curried
  println(cat4Carried)
  println(cat4Carried("a+")("b"))
  val cat5 = Function.uncurried(cat4Carried)
  println(cat5("a+", "b"))
}; {
  // # 函数式编程的数据结构
  {
    // ## 序列
    // 这是一个 trait
    val list1 = List("programming")
    val list2 = "people" :: "should" :: list1
    println(list2)
    // 或者向空队列添加
    // :: 方法意为 cons
    val list3 = "people" :: Nil
    println(list3)
    // 队列的连接
    println(list2 ++ list3)
    // seq 的构造方法是 +:
    val seq1 = Seq("1", "2")
    println("3" +: seq1)
    val vec1 = Vector("a", "b", "c")
    println("d" +: vec1)
    // 当前版本的 vector 已经是 immutable 的了，可以不用担心线程安全问题了
  }
  {
    // ## 映射表
    val stateCapitals = Map(
      "a" -> "alibaba",
      "t" -> "tencent"
    )
    // key -> value 实际上是调用了 Map.apply 方法
    println(stateCapitals)
    val lengths = stateCapitals map {
      kv => (kv._1, kv._2.length)
    }
    println(lengths)
    val stateCapitals1 = stateCapitals + ("b" -> "baidu")
    println(stateCapitals1)
    // stateCapitals 如果不添加括号可能先由于优先级问题变成字符串映射
    // 这些类型都暴露了可变和不可变类型
  }
  // ## 集合，略
  {
    // ## 遍历、映射、过滤、折叠和规约
    List(1, 2, 3, 4, 5) foreach { i => println("Int: " + i) }
    // 映射表的遍历也是键值对
    // def map[B,That](f:A=>B)(implicit bf: CanBuildFrom[Repr,B,That]):That
    object Combinators {
      def map[A, B](list: List[A])(f: (A) => B): List[B] = list map f
    }
    // map 可以实现 (A)=>(B) 的映射
  }
  // ## flatMap，类似调用 map 和 flatten
  {
    // ## filter
    /*
      drop 扔掉起始的几个元素
      dropWhile 扔掉元素直到某谓词值
      exists 是否有存在某谓词的
      filter
      filterNot 反 filter
      find 找到某个符合谓词的元素
      forall 检测是否全部符合某谓词
      partition 二划分
      take 取前 n 元素
      takeWhile 取元素直到
     */
  }
  {
    // ## 折叠
    val list = List(1, 2, 3, 4, 5, 6)
    println(list reduce (_ + _))
    println(list.fold(10)(_ * _))
    // reduce 不能够对空列表使用，需要使用 optionReduce
    /*
      fold 默认未指定方向，有初始值
      foldLeft 左折叠
      foldRight 右折叠
      /: 左折叠
      :\ 右折叠
      reduce 折叠集合，无定序，一般左折叠
      reduceLeft 左折叠
      reduceRight 右折叠
      optionReduce 可空折叠
      reduceLeftOption 可空左折叠
      reduceRightOption 可空右折叠
      aggregate 对后面元素进行操作，并聚合结果；
        将集合分为不同的分片，顺序遍历各个分片，用 seqop 更新计算结果
        最后对各个分片的计算结果调用 combop
      scan 扫描计算集合元素的前缀
      scanLeft 从左到右扫描
      scanRight 从右到左扫描
      product 累乘
      mkString 将集合中所有元素序列化到字符串中，可以添加前缀、分隔符和后缀
     */
  }
}; {
  // # 向左遍历和向右遍历
  // foldLeft 和 foldRight 的参数位置不同
  {
    // ## 尾递归和遍历无限集合
    // foldLeft 是尾递归的
    def myReduceLeft[A, B](s: Seq[A])(f: A => B): Seq[B] = {
      @annotation.tailrec
      def rl(accum: Seq[B], s2: Seq[A]): Seq[B] = s2 match {
        case head +: tail => rl(f(head) +: accum, tail)
        case _ => accum
      }

      rl(Seq.empty[B], s)
    }

    def myReduceRight[A, B](s: Seq[A])(f: A => B): Seq[B] = s match {
      case head +: tail => f(head) +: myReduceRight(tail)(f)
      case _ => Seq.empty[B]
    }
    // foldLeft 可以获得尾递归优化
    // 右遍历可以处理无限集合集合
    // scala 的 Stream 就是用来惰性处理无限集合的
    {
      import scala.math.BigInt

      // 书上的版本有误，需要添加 lazy 关键字，否则会因为作用域问题无法找到 fibs1 定义
      lazy val fibs1: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs1.zip(fibs1.tail).map(n=>n._1+n._2)
      fibs1 take 10 foreach (i=>println(s"$i "))
      // 除无限列表外，需要右遍历的可以先左遍历再反转
    }
  }
}; {
  // # 组合器：软件最佳组件抽象
  // 可以将组合器串联起来，用很少的代码完成复杂的功能
}; {
  // # 关于复制
  // scala 的不可变数据类型采用了分支系数为 32 的树状数据结构
  // 搜索和修改是 O(log32(N)) 复杂度的
}