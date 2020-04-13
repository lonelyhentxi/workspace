import scala.annotation.tailrec

1 to 10
// 闭集
1 until 10
// 右开集
1 to 10 by 3;
// 步长
// 负步长，其它类型略
{
  // # 方法声明
  // ## 方法默认值和命名参数列表，pylike 略
  import progscala.typelessdomore.shapes._
  val p1 = Point(x = 3.3, y = 4.4)
  // 可以删除 new，因为 case class 的 apply 自动实现了
  println(p1)
  val p2 = p1.copy(y = 6.6)
  println(p2)

  // ## 方法具有多个参数列表
  // 允许把函数两边的圆括号替换成方括号
  // 可以便于使用默认值，以及对齐美观
  // 便于进行类型推断
  // def m1[A](a: A, f: A=> String) = f(a) 无法推断类型
  def m2[A](a: A)(f: A => String) = f(a)

  m2(100)(i => s"$i + $i")
  // 可以使用最后一个参数列表来推断隐含参数
  // ## future 简介
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.util.{Success, Failure}

  def sleep(millis: Long) = {
    Thread.sleep(millis)
  }

  def doWork(index: Int) = {
    sleep((math.random() * 1000).toLong)
    index
  }

  (1 to 5) foreach { index =>
    // future api 允许获得隐含的 execution
    // 如果未指定，则默认 Implicits.global 作为执行上下文
    val future = Future {
      doWork(index)
    }
    future onComplete {
      case Success(answer: Int) => println(s"Success! returned: $answer")
      case Failure(th: Throwable) => println(s"FAILURE! returned: $th")
    }
  }

  sleep(1000)
  println("Finito!")

  // 嵌套方法的定义和递归
  def factorial(i: Int): Long = {
    // 该关键字对函数进行尾递归标注、申请和检查
    @tailrec
    def fact(i: Int, accumulator: Int): Long = {
      if (i <= 1) accumulator
      else fact(i - 1, i * accumulator)
    }

    fact(i, 1)
  }
  // scala 编译器对函数调用自身进行了尾递归优化，但是不会对 trampoline
  // 情形进行尾递归优化
  // JVM 并不会进行尾递归优化
  (0 to 5) foreach (i => println(factorial(i)))
}; {
  // # 推断类型信息
  // scala 需要支持子类多态，使得它难以进行全局类型推断
  /*
    需要显式类型注解的情况
    1. 声明了 var 或者 val 但没有进行初始化的变量
    2. 所有的方法参数
    3. 方法的返回值类型，在以下情况下必须：
      - 在方法中明显地使用了 return
      - 递归方法
      - 两个或多个方法重载，其中一个调用了另一个地重载方法，调用者需要显式类型注解
      - scala 推断出的类型比需要的更加宽泛
   */
  // 显而易见，可变参数一定需要时最后一个参数
  // 为避免函数返回类型被修改导致的调用方编译错误
  // 要注意开发 api 时，如果与客户端分开构建，应该显式地声明返回类型
  // 并且返回一个能返回的最通用类型
  // 要避免忘掉了等号的情况
  // 这种语法在 2.11 已经被废除
}; {
  // # 保留字，略
}; {
  // # 字面量
  // ## 整数字面量
  val a = 42
  val b = 0xff

  // val c = 077 八进制整数字面量
  // 数字溢出会报错
  // ## 浮点数字面量
  // Float 遵循 IEEE 754 32
  // Float 遵循 IEEE 754 64
  // ## 布尔型字面量，略
  // ## 字符字面量，略
  // ## 字符串字面量
  // 双引号转义略
  // 三重引号包含多行字符串，包含的字符串不进行转义
  // 结尾需要加空格，防止解析错误
  def hello(name: String) =
    s"""Welcome!
       Hello, $name,
       * (Gratutious Star!!!)
       |We're glad you're here.
       |  Have some extra whitesace.""".stripMargin
  // stripMargin 可以让 | 作为行首
  println(hello("Programming Scala"))
  // 或者可以使用 stripPrefix 或者 stripSuffix 方法
  println(
    """xxxCommon again!yyy
      |""".stripMargin.stripPrefix("xxx").stripSuffix("yyy"))
  // ## 符号字面量，类似 ES6 Symbol
  // ## 函数字面量
  val f1: (Int, String) => String = (i, s) => s + i
  val f2: Function2[Int, String, String] = (i, s) => s + i
  // ## 元组字面量
  val t1: (Int, String) = (1, "two")
  val t2: Tuple2[Int, String] = (1, "two")
  // -> 箭头操作符适用于生成 Tuple2
}
{
  // # Option、Some、None 避免使用 null
  val stateCapitals = Map(
    "a" -> "A",
    "b" -> "B",
    "c" -> "C",
  )

  println(stateCapitals.get("a").get)
  try {
    println(stateCapitals.get("d").get)
  } catch {
    case e: NoSuchElementException => println(
      e.toString
    )
  }
  println(stateCapitals.get("d").getOrElse("else"))
}; {
  // # 封闭类的继承
  // sealed abstract class Option[+A] ... {...}
  // 关键字 sealed 告诉编译器，所有的子类在同一个源文件中声明
};{
  // # 导入类型及其成员
  // java 导入可以使用 * 和 static 导入
  // * 和 _ 都可以被当作到导入通配符
  import java.awt._
  import java.io.File
  import java.io.File._
  import java.util.{Map,HashMap}
  // scala 的 import 几乎可以用在任何位置上
  // 可以导入时做重命名，也可以限制不想要类型的可见性
  def stuffWithBigInteger() = {
    import java.math.BigInteger.{
      ONE => _, // 屏蔽变量
    TWO, // 导入变量 TWO
    ZERO => JAVAZERO // 重命名
    }
    println(s"TWO: $TWO")
    println(s"ZERO: $JAVAZERO")
  }
  stuffWithBigInteger()
  // ## 导入是相对的
  // java 和 scala 重要的一点是 scala 的导入是相对的
  import scala.collection.mutable._
  import collection.immutable._ // scala 已经导入，不需要给出全路径
  import _root_.scala.collection.parallel._ // 从根开始的全路径
  // 必须保证问题库的所在路径被包含在 CLASSPATH 中
  // ## 包对象
  {
    // scala 支持包对象这一特殊类型的、作用域为包层次的对象。
    // 它像普通对象一样声明，但和普通对象有着如下示例所展示的不同点
    // 见 example.json.package.json
    /*
      1. 文件名必须为 package.json 按照惯例目录名应该和包名相同
      2. 声明上层包的作用域
      3. 使用 package 关键字给包名之后的对象命名，在这里对象名为 json
      4. 适合暴露给客户端的成员
     */
  }
}; {
  // # 抽象类型和参数化类型
  // scala 抽象化类型采用 [] 表示
  // +A 为协变，-A 为逆变, 类似 C#
  // type 使用方式为关联类型，类似 rust
  // 见 abstract-types.sc
}