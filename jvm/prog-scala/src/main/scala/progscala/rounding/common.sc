{
  import scala.language.postfixOps // 消除后缀表示法警告，有作用域，更合适
  // # 操作符重载
  // scala 操作符是一个函数
  // scala 在非特殊情况下允许除了数字和字母之外的字符
  println(1 + 2 == 1.+(2))
  // 等价
  // 中缀表示法表示单参数方法时，可以省略点号和括号
  // 调用无参数方法也可以省略点号，成为后缀表示法，但是因为有歧义，2.10 后设为可选属性
  println(1.toString.equals(1 toString));
  {
    // 允许出现的在标识符中的字符
    /* 1. 可用的字符：
       除了括号类、分隔符之外，其他所有可打印的 ASCII 字符如字母、数字、下划线
       和美元符号，还允许使用 \u0020 到 \u007f 之间的字符。
       2. 不能使用保留字：
       3. 普通的标识符：
       还允许使用 unicode 格式的字符，美元符不应该单独使用
       编译器会将下划线之后空格之前的所有字符视为标识符的一部分
       4. 普通的标识符 —— 操作符：
       加入某一标识符以操作符开头，那么后面的字符必须是操作符字符
       5. 使用反引号定义标识符：
       保留字和有歧义的标识符可以使用反引号注明去歧义
       6. 模式匹配标识符：
       小写字母开头的标识符会解析为变量标识符
       大写开头的标识符会解析为常亮标识符
     */
    //
  }
  {
    // 语法糖
    // 这些设计有助于创造自己的语法糖
    // 不要常使用不利于可读性
  }
}; {
  // # 无参数方法
  // 为了和 java 的互调用性
  // 定义方法时没有括号，则必须没有括号调用
  // 定义方法时有括号没有或者有都行
  println(s"size:${List(1, 2, 3).size}")

  def isEven(n: Int) = (n % 2) == 0
  List(1,2,3,4) filter isEven foreach println
  List(1,2,3,4).filter((i:Int)=>isEven(i)).foreach((i:Int)=>println(i))
  List(1,2,3,4).filter(i=>isEven(i)).foreach(i=>println(i))
  List(1,2,3,4).filter(isEven).foreach(println)
  // 第二行到第三行是类型自动推导
  // 第三行到第四行是函数指针和 lambda 等价抽象
  // 第四行到第一行是省略括号
  // 对于多个参数编译器会困惑，需要标注，单参数可以推断
}; {
  // # 优先级规则
  // scala 中冒号结尾的方法都与右边的对象绑定
  val lst = List('b','c','d')
  println(lst)
  println('a' :: lst)
}; {
  // # 领域特定语言
  // 见 test 的 NerdFindSpec.sc
}; {
  // # scala 中的 if 语句
  // 具有返回值
  // 返回值类型是返回所有分支的最小上界类型
}; {
  // # scala 中的 for 推导式
  {
    // ## for 循环
    // 不返回值
    val chars = List("a","b","c")
    for(ch <- chars) println(ch)
  }; {
    // ## 生成器表达式
    // <- 语法会对迭代器迭代生成内容
  }; {
    // ## 保护式：筛选元素
    val ints = List(1,2,3)
    for(int <- ints if int%2!=0) println(int)
    // 可以添加多个保护式
  }; {
    // ## yielding
    // generator，返回值
    val ints = List(1,2,3)
    (for(int <- ints if int%2!=0) yield int) foreach println
    // for 推导式单一表达式时用圆括号，多表达式大括号
  }; {
    // ## 拓展作用域与值定义
    val chs = List('a','b','c')
    for {
      ch <- chs
      ch_upper = ch.toUpper
    } println(ch_upper)
    // 该操作符可以从 option 提出值,<- None 不会被处理, 直接跳过该循环
    // 从容器中提取值，使用箭头，不需要迭代，使用等号
    val ops = List(Some('a'),None,Some('b'))
    for {
      op <- ops
      unwrapped <- op
      upper = unwrapped.toUpper
    } println(s"op:$op,upper:$upper")
  }
}
// # 其他循环结构
// while 和 do-while 和 JAVA 等相同
// # 条件操作符
// 和 JAVA 相同
{
  // # 使用 try、catch、final 子句
  // 见 TryCatch.scala
  // scala 推崇通过函数式结构和强类型以减少对异常和异常处理的依赖
  // scala 将检查型异常视为非检查型
  // 使用 finally 以进行清理逻辑
  // 可以使用 throw new MyBadException() 方式
  // 使用 case 可以不 new
  // 为了自动资源管理，可以使用 scalaARM 独立项目
}; {
  // 名字调用和值调用
  // 见 TryCatchArm.scala
  // ## manage.apply 方法
  // R <: Closable 表示 Closable 接口中定义了对应方法并且实现了该接口
  // 结构化类型代价较高，反射机制造成许多系统开销，被列为可选特性
  // resource 是个传名参数，暂且将其视为一个在调用时应省略括号的函数
  // scala 支持延迟计算，因此提供了传名参数
  // 传名参数参数的求值惰性计算
};{
  // # 枚举
  // scala 在标准库中定义了 Enumeration 类
  // 见 enumeration.sc
}; {
  // 可插入字符串
  // ${xxx} 标准插入
  // $xxx 无歧义插入
  // $$ 打印 $
  // 使用 f 开始还可能格式化
  // scala 编译器会为某些语境中对 Java 字符串进行封装并提供了一些额外的方法
}; {
  // # trait: scala 语言的接口和混入
  /*
     目前可以将其视为允许将声明方法实现的接口
     使用 trait 时可以声明示例字段并选择是否定义这些字段
     也可以声明和定义类型
     实现混入的特征
   */
}