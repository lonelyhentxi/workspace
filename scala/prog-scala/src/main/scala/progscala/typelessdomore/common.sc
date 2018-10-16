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
  val p1 = Point(x=3.3,y=4.4)
  // 可以删除 new，因为 case class 的 apply 自动实现了
  println(p1)
  val p2 = p1.copy(y=6.6)
  println(p2)
  // ## 方法具有多个参数列表
  // 允许把函数两边的圆括号替换成方括号
  // 可以便于使用默认值，以及对齐美观
  // 便于进行类型推断
  // def m1[A](a: A, f: A=> String) = f(a) 无法推断类型
  def m2[A](a: A)(f: A=> String) = f(a)
  m2(100)(i => s"$i + $i")
  // 可以使用最后一个参数列表来推断隐含参数
  // ## future 简介
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.util.{Success,Failure}

  def sleep(millis: Long) = {
    Thread.sleep(millis)
  }

  def doWork(index: Int) = {
    sleep((math.random()*1000).toLong)
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
      case Failure(th:Throwable) => println(s"FAILURE! returned: $th")
    }
  }

  sleep(1000)
  println("Finito!")
  // 嵌套方法的定义和递归
  def factorial(i: Int): Long = {
    // 该关键字对函数进行尾递归标注、申请和检查
    @tailrec
    def fact(i:Int,accumulator: Int): Long = {
      if (i <= 1) accumulator
      else fact(i - 1, i * accumulator)
    }
    fact(i,1)
  }
  // scala 编译器对函数调用自身进行了尾递归优化，但是不会对 trampoline
  // 情形进行尾递归优化
  // JVM 并不会进行尾递归优化
  (0 to 5) foreach (i => println(factorial(i)))
}
{
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
}
{
  // # 保留字
}
