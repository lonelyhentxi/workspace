// 特征
// java 中，接口的类使用了大量的样板代码实现接口的大量功能
// trait 帮助实现混入
{
  // # java 8 的接口
  // java 8 中可以在接口中定义方法，被称为 default 方法
  // java 8 中接口行为更接近于 scala 中的 trait
  // java 8 中接口只能定义静态字段，trait 可以定义实例字段吗
}; {
  // # 混入 trait
  // button callbacks 见 ui/ButtonCallbacks
  // observer 见 observer/Observer
  // button 见 ui/Button
  // 混入了 observer trait 的 button 见ui/ObservableButton
  {
    import progscala.traits.ui._
    import progscala.traits.observer._

    class ButtonCountObserver extends Observer[Button] {
      var count = 0

      override def receiveUpdate(state: Button): Unit = count += 1
    }

    val button = new ObservableButton("Click me!")
    val bco1 = new ButtonCountObserver
    val bco2 = new ButtonCountObserver
    button addObserver bco1
    button addObserver bco2
    (1 to 5) foreach (_ => button.click())
    println(bco1.count == 5)
    println(bco2.count == 5)
  }
  // 如果未声明拓展了其他类，必须要在第一个 trait 前面使用 extends，后面可以使用 with
}; {
  // # 可堆叠的特征
  // 可以通过进一步的改进方案提高代码的可重用性，使代码能同时使用多个 trait
  import progscala.traits.ui2._
  import progscala.traits.observer._
  {
    val button = new Button("Click Me!") with ObservableClicks

    class ClickCountObserver extends Observer[Clickable] {
      var count = 0

      override def receiveUpdate(state: Clickable): Unit = count += 1
    }

    val bco1 = new ClickCountObserver
    val bco2 = new ClickCountObserver
    button addObserver bco1
    button addObserver bco2
    (1 to 5) foreach (_ => button.click())
    println(bco1.count == 5)
    println(bco2.count == 5)
    println("Success!")
  }
  {
    val button =
      new Button("Click Me!") with ObservableClicks with VetoableClicks {
        override val maxAllowed = 2
      }

    class ClickCountObserver extends Observer[Clickable] {
      var count = 0

      def receiveUpdate(state: Clickable): Unit = count += 1
    }

    val bco1 = new ClickCountObserver
    val bco2 = new ClickCountObserver

    button addObserver bco1
    button addObserver bco2

    (1 to 5) foreach (_ => button.click())

    println(bco1.count == 2, s"bco1.count ${bco1.count} == 2")
    println(bco2.count == 2, s"bco2.count ${bco2.count} == 2")
    println("Success!")
  }
  // 混入有相同的方法的特征，调用时会按照声明顺序从右到左调用
  // scala 使用线性化算法 linearization (计算优先级)
  // 解决具体类继承树中 trait 和类的优先级问题
}; {
  // # 构造 trait
  // trait 可以继承自 trait 甚至类
  // trait 构造时无法向父类构造函数传递参数
  // 每次创建使用了 trait 的实例时，特征体都会被执行
  trait T1 {
    println(s"  in T1: x = $x")
    val x=1
    println(s"  in T1: x = $x")
  }

  trait T2 {
    println(s"  in T2: y = $y")
    val y="T2"
    println(s"  in T2: y = $y")
  }

  class Base12 {
    val b="Base12"
    println(s"  in Base12: b = $b")
  }

  class C12 extends Base12 with T1 with T2 {
    val c="C12"
    println(s"  in C12: c = $c")
  }

  println(s"Creating C12:")
  new C12
  println(s"After Creating C12")
  // 不要在 trait 中声明那些无法再初始化时指定合适默认值的具体字段
  // 如果需要声明这些类字段，使用抽象字段或者含有构造函数的类
}; {
  // # 选择类还是 trait
  // 大多数时候表现为 is-a 的时候使用继承
  // 良好的面向对象设计需要遵循以下通用原则：
  // 一旦完成构造，实例便应该处于某种已知的合法状态
}