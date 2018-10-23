import scala.concurrent.ExecutionContext {
  // # 简介
  // 隐式可以减少代码，能够向已有类型中注入新的方法，也能够创建领域特定语言
  // 隐式一旦进入作用域，编译器就能够执行该对象以生成方法参数或转化成对应类型
}; {
  // # 隐式参数
  // 永远不要使用 Float 表示货币
  {
    def calcTax(amount: Float)(implicit rate: Float): Float = amount * rate

    object SimpleStateSalesTax {
      implicit val rate: Float = 0.05f
    }
    case class ComplicatedSalesTaxData(baseRate: Float, isTaxHoliday: Boolean, storeId: Int)
    object ComplicatedSalesTax {
      private def extraTaxRateForStore(id: Int): Float = {
        0.0F
      }

      implicit def rate(implicit cstd: ComplicatedSalesTaxData): Float = {
        if (cstd.isTaxHoliday) 0.0F
        else cstd.baseRate + extraTaxRateForStore(cstd.storeId)
      }
    }
    {
      import SimpleStateSalesTax.rate
      val amount = 100F
      println(s"Tax on $amount = ${calcTax(amount)}")
    }
    {
      import ComplicatedSalesTax.rate
      implicit val myStore = ComplicatedSalesTaxData(0.06f, false, 1010)
      val amount = 100f
      println(s"Tax on $amount = ${calcTax(amount)}")
    }
  }
  {
    // ## 调用 implicitly 方法
    // 定义在 Predef 中
    import math.Ordering

    case class MyList[A](list: List[A]) {
      def sortBy1[B](f: A => B)(implicit ord: Ordering[B]): List[A] =
        list.sortBy(f)(ord)

      def sortBy2[B: Ordering](f: A => B): List[A] =
        list.sortBy(f)(implicitly[Ordering[B]])
    }

    val list = MyList(List(1, 3, 5, 2, 4))
    list sortBy1 (i => -i)
    println(list)
    list sortBy2 (i => -i)
    println(list)
  }
}; {
  // # 隐式参数适用的场景
  {
    // ## 执行上下文
    // apply[T](body: => T)(implicit executor: ExecutionContext):Future[T]
    // 通过隐式参数指定上下文
  }
  {
    // ## 功能控制
    // 隐式参数可以类似令牌控制系统功能
    /*
    def createMenu(implicit session: Session): Menu = {
      var defaultItems = List(helpItem, searchItem)
    }
    */
  }
  {
    // ## 限定可用实例
    // 应用 scala api
    // scala API 运用一种常见的手法，将一个构建器作为隐式参数传入方法中
    // 该构造器知道如何构造一个同种类型的新容器
  }
}