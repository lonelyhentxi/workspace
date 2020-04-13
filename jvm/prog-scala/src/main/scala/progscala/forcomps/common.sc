// 深入学习 for 推导式
{
  // # 内容回顾：for 推导式组成元素
  // for 推导式中包含一个或者多个表达式生成器
  // 外加可选的保护表达式以及值定义
  // 推导式的输出可以用于生成新的容器
  // 也可以在遍历时执行具有副作用的代码块
}; {
  // # for 推导式：内部机制
  {
    val states = List("Alabama", "Alaska", "Virginia", "Wyoming")

    for {
      s <- states
    } println(s)
    states foreach println
    // 不含 yield 的简单 for 推导式
  }
  {
    // 带 yield 的简单 for 推导式
    val states = List("Alabama", "Alaska", "Virginia", "Wyoming")

    println(for {
      s <- states
    } yield s.toUpperCase)
    println(states map (_.toUpperCase()))
  }
  {
    // 多个生成器
    val states = List("Alabama", "Alaska", "Virginia", "Wyoming")

    println(for {
      s <- states
      c <- s
    } yield s"$c-${c.toUpper}")

    println(states flatMap (_.toSeq map (c => s"$c-${c.toUpper}")))
  }
  {
    // 增加保护式
    val states = List("Alabama", "Alaska", "Virginia", "Wyoming")

    println(for {
      s <- states
      c <- s
      if c.isLower
    } yield s"$c-${c.toUpper} ")
    println(states flatMap (_.toSeq withFilter (_.isLower) map (c => s"$c-${c.toUpper}")))
  }
  {
    // 定义变量
    val states = List("Alabama", "Alaska", "Virginia", "Wyoming")

    println(for {
      s <- states
      c <- s
      if c.isLower
      c2 = s"$c-${c.toUpper} "
    } yield c2)

    println(states flatMap (_.toSeq withFilter (_.isLower) map { c =>
      val c2 = s"$c-${c.toUpper} "
      c2
    }))
  }
}; {
  // # for 推导式的转化规则
  // scala 中要做的第一件事是将 for 推导式的左边转化为模式匹配, 并检查是否能匹配成功
  {
    // pat <- expr
    // pat <- expr.withFilter { case pat => true; case _ => false }
  }
  {
    // for (pat<-expr) yield expr2
    // expr map { case pat => expr2 }
  }
  {
    // 包含多个生成器的 yield 会逐层展开
    // for(pat1 <- expr1; pat2 <- expr2; ...) yield exprN
    // expr1 flatMap { case pat1 => for(pat2 <- expr2 ...) yield exprN }
    // 对于无 yield 的 for 循环，flatMap 替换成 foreach
    // 保护式生成器生成 withFilter
  }
  {
    // pat1 <- expr1; pat2 = expr2
    /*
      (pat1,pat2) <- for {
        x1 @ pat1 <- expr1
      } yield {
        val x2 @ pat2 = expr2
        (x1,x2)
      }
     */
  }
  {
    val map = Map("one" -> 1, "two" -> 2)
    val list1 = for {
      (key, value) <- map
      i10 = value + 10
    } yield (i10)
    println(list1)
    // 翻译后的语句
    val list2 = for {
      (i, i10) <- for {
        x1@(key, value) <- map
      } yield {
        val x2@i10 = value + 10
        (x1, x2)
      }
    } yield i10
    println(list2)
  }
}; {
  // # option 以及其他一些容器类型
  {
    // ## option 容器
    // 略，见 rust、haskell
  }
  {
    // ## either： option 类型的逻辑拓展
    // either 可以为我提供信息 —— 为什么不返回值
    // Left 表示错误标志，Right 表示正确标志
    {
      def positive(i: Int): Either[String,Int] =
        if (i > 0) Right(i) else Left(s"nonpositive number $i")

      val res1= for {
        i1 <- positive(5).right
        i2 <- positive(10 * i1).right
        i3 <- positive(25 * i2).right
        i4 <- positive(2  * i3).right
      } yield i1 + i2 + i3 + i4
      println(res1)

      val res2 = for {
        i1 <- positive(5).right
        i2 <- positive(-1 * i1).right
        i3 <- positive(25 * i2).right
        i4 <- positive(-2 * i3).right
      } yield i1 + i2 + i3 + i4
      println(res2)
    }
    {
      val l: Either[String,Int] = Left("boo")
      val r: Either[String,Int] = Right(12)
      println(l.left)
      println(l.right)
      println(r.left)
      println(r.right)
      println(l.left.map(_.size))
      println(r.left.map(_.size))
      println(l.right.map(_.toDouble))
      println(r.right.map(_.toDouble))
    }
    // 抛出异常会破坏引用的透明性
    // java 的可检查异常已经被证明失败
  }
  {
    // ## try 类型
    import scala.util.{Try,Success,Failure}

    def positive(i:Int): Try[Int] = Try {
      assert(i>0,s"nonpositive number $i")
      i
    }

    val l4 = for {
      i1 <- positive(5)
      i2 <- positive(10 * i1)
      i3 <- positive(25 * i2)
      i4 <- positive(2 * i3)
    } yield i1 + i2 + i3 + i4
    println(l4)
    val l5 = for {
      i1 <- positive(5)
      i2 <- positive(-10 * i1)
      i3 <- positive(25 * i2)
      i4 <- positive(-2 * i3)
    } yield i1 + i2 + i3 + i4
    println(l5)
  }
  {
    // scalaz 提供的 validation 类
    // 会提示所有 validation 出的错误
  }
}