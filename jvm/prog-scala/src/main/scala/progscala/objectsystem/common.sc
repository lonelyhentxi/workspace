// scala 对象系统 I
{
  // # 参数化类型：继承转化
  // Scala 参数化类型和 Java 参数化类型的重要区别在于继承机制如何工作
  // 协变、逆变关系略，见 C#
  // scala 类型转化使用的是转化标记，+、-来表示
  {
    // ## Hood 下的函数
    List(1, 2, 3, 4) map (i => i + 3)
    // 实际上是一个语法糖，编译器将其转化为 scala.Function1 的匿名子类
    // Java 8 已经解除了对裸函数的限制，但是 scala 为了保持对之前版本 JVM 的兼容，仍然这样设置
    class CSuper {
      def msuper() = println("csuper")
    }
    class C extends CSuper {
      def m() = println("c")
    }
    class CSub extends C {
      def msub() = println("csub")
    }

    var f: C => C = (c: C) => new C
    f = (c: CSuper) => new CSub
    // 变异标记只有在类型声明中的类型参数才有意义，对参数化的方法没有意义，因为该标记影响的是子类继承行为
    // 编译器会检查你所用的编译标记是否有效、如果试图在自定义的函数加上错误的标记，就会发生编译错误
  }
  {
    // ## 可变类型的变异
    // 可变类型只允许非变异类型
    // 对于可变属性，getter 是逆变，setter 是协变，没有既协变又逆变的类型
  }
  {
    // ## scala 和 java 中的变异
    // java 的类型系统存在两个缺点
    /*
      - 库的设计者应该负责类型的编译行为并编写进库中，但是是库的用户承担这个责任
      - Java 程序员很可能指定错误的变异注释，导致不安全的代码
     */
    // Java 的类型系统另外一个问题是 —— Array 是协变的
  }
}; {
  // scala 的类型层次结构
  // Any 处于类型结构树的根部位置，Any 没有父类
  // AnyVal，价值类型和价值类的父类
  // AnyRef，所有引用类型的父类
  // 通用特征，新引入的 trait 类型
  // 从 2.10 开始，编译器对所有的 scala 引用类型混入了名为 ScalaObject 的 marker 特征，2.11 之后，该 trait 将会被移除
}; {
  // # 闲话 Nothing（以及 Null）
  // Null 同时为 abstract 和 final，但运行时环境提供了一个实例 null
  // Nothing 没有实例
  // Nothing 是所有类型的子类型
  // Scala 为空列表提供了一个专门的类型 Nil
  // object Nil extends List[Nothing] with Product with Serializable
  // 因此 Nil 是所有 List 的子类型
  /*
    Nothing 的用途，表示所有类型的子类
    其他用途表示终止程序
   */
}; {
  // # product、case 类和元组
  // 用户定义的 case 类会混入 scala.Product 特征，提供了几个关于实例字段的通用方法
  case class Person(name: String, age: Int)
  val p = Person("Dean", 29)
  println(p.productArity)
  println(p.productElement(0))
  p.productIterator foreach println
  // 当下版本的 tuple 中已经去除了对长度的限制
}; {
  // # Predef 对象
  // 为了方便起见，只要编译 scala 程序，就会自动引入 Predef 包
  {
    // ## 隐式转换
    // 定义了很多隐式转换，例如值类型的装箱和拆箱
    // 最后，scala 2.10 中定义了一组隐式转换，可以防止 null 用来赋值值类型
  }
  {
    // ## 类型定义
    // 为了鼓励使用不可变集合，Predef 为最常用的不可变集合定义了别名
    // 用于两元组合三元组的两个转换别名在 2.11 中被废弃
    // 以及支持类型推断的其他类型
  }
  {
    // ## 条件检查方法
    /*
      assert 测试条件是否为真，否则抛出 java.lang.AssertionError 异常
      assert 测试条件是否为真，参数被转为错误信息字符串
      assume 表示代码块为真时，条件才为真
      assume 转为错误信息字符串
      require 其含义是调用方是否满足某些条件，也可以表示某个实现不能得出特定的结果
      require 添加的参数转为错误信息字符串
      @elidable 将阻止相应标记的参数大于编译时确定的阈值，否则对代码中的定义不产生字节码
      便于编译器设置级别
     */
  }
  {
    // ## 输入输出方法
    // printXxx
    // readType
  }
  {
    // ## 杂项方法
    // ??? 方法，前面已经陈述，略
    // identity[A](x:A):A 直接返回参数 x
    // 在将方法传给组合器时如果不需要进行修改
    // 就可以传入 identity
    // def implicitly[T](implicit e:T):T
    /*
      当隐式参数列表使用简写 [T:M] 时
      编译器会添加形式为 implicit arg: M[T] 的隐式参数列表
      调用 implicitly 可以返回参数 arg
     */
  }
}; {
  // # 对象的相等
  {
    // ## equals 方法
    // equals 的行为类似 Java 中的 equals 方法和 Ruby 里的 eq? 方法
  }
  {
    // ## == 和 != 方法
    // scala 里测试的是值得相等性，null 调用不会引发错误
  }
  {
    // ## eq 和 ne 方法
    // 测试引用的相等性
  }
  {
    // ## 数组相等和 sameElements 方法
    // 比较序列的相等性
  }
}
