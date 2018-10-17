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