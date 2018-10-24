import scala.concurrent.ExecutionContext
import scala.util.parsing.json.JSONObject; {
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
  {
    // ## 应用 scala API，见 java-database-api
  }
  {
    // ## 隐式证据
    // 隐式对象对允许的对象进行限定，而这些允许的类型不具有共同的超类
    // 有时候只需要限定需要的类型，并不需要提供额外的处理 —— 类似 PhatomData
    /*
      trait TraversableOnce[+A] ... {
        ...
        def toMap[T,U](implicit ev: <:<[A,(T,U)]: immutable.Map[T,U]
        ...
      }
     */
    val l1 = List("one" -> 1, "two" -> 2, "three" -> 3)
    println(l1.toMap)
    // 只有满足 A<: (T,U) 才会执行，<:< 类型取决于 <: 方法
    // predef 中还定义了一个名为 =:= 的证据类型
    // 它可以证明两个类型之间的等价关系
  }
  {
    // ## 绕开类型擦除带来的限制
    // 使用隐式证据证明满足某些特定的类型约束
    // 可以避免 JVM 对泛型进行的类型擦除问题
    object M {

      implicit object IntMarker

      implicit object StringMarker

      def m(seq: Seq[Int])(implicit i: IntMarker.type): Unit = {
        println("m1")
      }

      def m(seq: Seq[String])(implicit s: StringMarker.type): Unit = {
        println("m2")
      }
    }
    // 不推荐使用隐式 Int 和 String 值
    // 因为和其它类型相比，这些类型更有可能会在多处定义其对应的隐式对象
  }
  {
    import scala.annotation.implicitNotFound
    // ## 改善报错信息
    @implicitNotFound(msg = "Can find ${From}")
    trait MyTrait1[-From] {}
    // 也解释了为什么构建隐式时应该使用自定义类型
    println(" ")
  }
  {
    // ## 虚类型
    // phantom type
    sealed trait PhantomType1
    // 这些密封特征本身不包含任何数据，而且没有实现这些 trait 的类
    // 他们被密封了，不能在其它文件中使用这些 trait
    println(" ")
  }
  {
    // ## 隐式参数遵循的规则
    // 只有最后一个参数列表中允许出现隐式参数，也适用于只有一个参数列表得情况
    // implicit 只能出现在参数列表的最左边
    // 加入列表为 implicit 关键字开头，那么所有的参数都是隐式的
  }
}; {
  // # 隐式转换
  implicit final class AnotherArrowAssoc[A](val self: A) {
    def -->[B](y: B): Tuple2[A, B] = Tuple2(self, y)
  }
  println("one" --> 1);
  // 编译器发现试图对 String 对象执行 --> 方法
  // 由于 String 未定义 --> 方法，试图查找隐式方法
  // 发现了 AnotherArrowAssoc 类
  // 创建对象，传入 “one” 作为参数
  // 之后应用方法
  // 2.10 变成了可选特性
  // 加入调用的对象和方法成功通过了类型检查，那么转换不会被执行
  // 编译器只会考虑 implicit 关键字的类和方法
  // 只会考虑作用域内
  // 隐式方法只能直接转换，不能通过中间的间接方法
  // 如果适用多条转换，为了避免二义性将不会执行
  {
    // ## 构建独有的字符串插入器
    import org.json.JSONObject

    object Interpolators {
      implicit class jsonForStringContext(val sc: StringContext) {       // <1>
        def json(values: Any*): JSONObject = {                           // <2>
          val keyRE = """^[\s{,]*(\S+):\s*""".r                          // <3>
        val keys = sc.parts map {                                      // <4>
          case keyRE(key) => key
          case str => str
        }
          val kvs = keys zip values                                      // <5>
          new JSONObject(kvs.toMap)                                          // <6>
        }
      }
    }

    import Interpolators._

    val name = "Dean Wampler"
    val book = "Programming Scala, Second Edition"

    val jsonobj = json"{name: $name, book: $book}"                       // <7>
    println(jsonobj)
  }
  {
    // ## 表达式问题
    // 在不修改源码的情况下拓展模块的期望被称为表达式问题
    // 单一职责原则鼓励我们在定义抽象及实现实体时，提供单一的行为
    // 在实际场合时要组合、混入对象
    // 为此可以使用类型类
  }
}; {
  // # 类型类模式
  case class Address(street: String, city: String)
  case class Person(name: String, address: Address)

  trait ToJSON {
    def toJSON(level: Int = 0): String

    val INDENTATION = "  "
    def indentation(level: Int = 0): (String,String) =
      (INDENTATION * level, INDENTATION * (level+1))
  }

  implicit class AddressToJSON(address: Address) extends ToJSON {
    def toJSON(level: Int = 0): String = {
      val (outdent, indent) = indentation(level)
      s"""{
         |$indent"street": "${address.street}",
         |$indent"city":   "${address.city}"
         |$outdent}""".stripMargin
    }
  }

  implicit class PersonToJSON(person: Person) extends ToJSON {
    def toJSON(level: Int = 0): String = {
      val (outdent, indent) = indentation(level)
      s"""{
         |$indent"name":    "${person.name}",
         |$indent"address": ${person.address.toJSON(level + 1)}
         |$outdent}""".stripMargin
    }
  }

  val a = Address("1 Scala Lane", "Anytown")
  val p = Person("Buck Trends", a)

  println(a.toJSON())
  println()
  println(p.toJSON())
  // scala 不允许同时使用 implicit 和 case 关键字
  // 类型类提供的多台成为特设多态
}; {
  // # 隐式所导致的技术问题
  // 花大量时间编写隐式需要的代码，编译器也要花时间处理
  // 封装类型会引入额外的中间层，会有额外开销
  // 一般应该利用多态分发任务，也不建议使用 switch 对对象类型进行判断
  // 无论何时，都要为隐式转换类型指定返回类型
}; {
  // # 隐式解析规则
  // scala 会解析无需输入前缀路径的类型兼容隐式类
  // import 引入的隐式值优先级会高于当前作用域的隐式值
}; {
  // # scala 内置的多种隐式
  // Predef 中定义了大多数的隐式定义
  // 其中的一些隐式定义包含了 @inline 标记，建议内联化
  // @noinline 标记可以阻止内联化
  // 该对象中还定义许多转化成 Java 对象的方法
  // 事实上，处于性能的考虑可能会避免执行类型转换
}