// scala 面向对象初步
// scala 提倡小处用函数式编程、大处用面向对象编程
{
  // # 类和对象初步
  // 类用关键字 class 声明，而单例对象用 object 关键字声明
  // 在类前加上 final，可以避免从一个类中派生出其他类
  // abstract 关键字阻止类的实例化
  // 实例可以使用 this 指代本身，scala 中很少看到，因为 scala 中没有样板构造函数
  // 实例的状态是实例所有字段内当前值得总和
  // method 是和实例绑定的函数
  // scala 允许方法重载
  // JVM 由于类型擦除，不允许定义不同泛型内部不同，在scala中可以使用type或者隐式参数实现
  // 与 java 不同，如果方法有参数列表，方法可以与类的字段同名
  // 同文件定义的类和对象相互伴随，对于 case 类，编译器自动生成一个伴随对象
}; {
  // # 引用和值类型
  // 引用类型实例使用 new 创建，是 AnyRef 的子类
  // 值类型是 AnyVal 的子类
  // apply 必须使用 new 或者字面量方法创建
  // 值类型没有构造器
  // scala 最大限度的减少包装过的引用类型
}; {
  // # 价值类
  // 对于值类型的装箱会导致变成引用类型，失去原生类型良好性能
  // scala 2.10 推出了价值类和附带特性 —— 通用特征
  {
    import progscala.basicoop.Dollar
    val benjamin = new Dollar(100)
    println(benjamin)
    // value class 不能作为本地 class
    /*
      只能有一个 val 参数
      参数的类型不能是价值类本身
      被参数化时，不能使用 @specialized 标记
      价值类没有定义其他构造器
      价值类只定义了一些方法，没有定义其他的 val 和 var 变量
      不能重载 equals 和 hashCode 方法
      价值类定义没有嵌套的特征、类和对象
      价值类不能继承
      价值类只能继承自通用特征
      价值类必须是对象可引用的一个顶级类型或对象的一个成员
     */
  }
  {
    // 由于 JVM 的限制，通用特征有时候会触发实例化
    /*
      价值类实例被传递给函数做参数，且该函数作为通用特征且需要被实现 —— dyn trait
      价值类的实例被赋值给数组
      价值类的类型被作为类型参数
     */
    import progscala.basicoop.Wrapper
    val w = new Wrapper(3)
    w.print()
  }
  {
    import progscala.basicoop.RichInt._

    println(1.toHexString1)
    // 可以使用 implicit 实现拓展方法
  }
}; {
  // # 父类
  // scala 同样只能单继承
  // Any 没有父类
  // 类继承 trait 时也用 extend 表示
  // trait 继承 trait 也用 extend
}; {
  // # scala 的构造器
  // scala 将主构造器与零个或多个辅助构造器分开
  // 辅助构造被命名为 this，它的第一个表达式必须调用主构造器或者其他辅助构造器
  // 被调用的构造器必须先于当前构造器出现
  case class Address(street: String, city: String, state: String, zip: String) {

    def this(zip: String) = // <1>
      this("[unknown]", Address.zipToCity(zip), Address.zipToState(zip), zip)
  }

  object Address {

    def zipToCity(zip: String) = "Anytown" // <2>
    def zipToState(zip: String) = "CA"
  }

  case class Person(
                     name: String, age: Option[Int], address: Option[Address]) { // <3>

    def this(name: String) = this(name, None, None) // <4>

    def this(name: String, age: Int) = this(name, Some(age), None)

    def this(name: String, age: Int, address: Address) =
      this(name, Some(age), Some(address))

    def this(name: String, address: Address) = this(name, None, Some(address))
  }

  val a1 = new Address("1 Scala Lane", "Anytown", "CA", "98765")
  println(a1)

  val a2 = new Address("98765")
  println(a2)

  println(new Person("Buck Trends1"))

  println(new Person("Buck Trends2", Some(20), Some(a1)))


  println(new Person("Buck Trends3", 20, a2))

  println(new Person("Buck Trends4", 20))
  // 通过强制让所有次级构造器都调用主构造器
  // 可以将代码最小化
  // 并且确保新实例初始化逻辑的一致性
  // 通过结合 Option 和默认值，可以简化库的开发者所需的维护负担
  // 编译器不会自动为 case 类的次级构造器创建 apply 方法，因此需要 new
  // 如果总是在伴随对象中创建对应的 apply 方法就可以得到便利的构造器
}; {
  // # 类的字段
  // 在主构造函数参数前面可以加上 val 或者 var 关键字
  // 可以转化成实例的一个字段，对于 case 类，val 是默认的
  // scala 偷偷创建了 java 中干的事情
  class Name1(var value: String)
  class Name2(s: String) {
    // 可变变量
    private var _value: String = s

    // 读方法
    def value: String = _value

    // 写方法
    def value_=(newValue: String): Unit = _value = newValue
  }
  // 如果 val 那么就不会生成写方法
  // 如果向构造器传递参数时不希望成为类的字段，就不要传递 val 或者 var
}; {
  // # 统一访问原则
  // scala 没有遵循 javaBeans 约定规范，getValue、setValue 方法
  // 使得不了解实现的用户可以同一访问访问器和字段
  {
    // ## 一元方法
    // 可以使用 xxx_= 使用一元写访问器
  }
  {
    case class ZipCode(zip: Int, extension: Option[Int] = None) {
      // 使用 require 方法可以验证输入
      require(valid(zip, extension),
        s"Invalid Zip+4 specified: $toString")

      protected def valid(z: Int, e: Option[Int]): Boolean = {
        if (0 < z && z <= 99999) e match {
          case None => validUSPS(z, 0)
          case Some(e) => 0 < e && e <= 9999 && validUSPS(z, e)
        }
        else false
      }

      protected def validUSPS(i: Int, e: Int): Boolean = true

      override def toString =
        if (extension.isDefined) s"$zip-${extension.get}" else zip.toString
    }

    object ZipCode {
      def apply(zip: Int, extension: Int): ZipCode =
        new ZipCode(zip, Some(extension))
    }
    ZipCode(12345)

    ZipCode(12345, Some(6789))

    ZipCode(12345, 6789)

    try {
      ZipCode(0, 6789)
    } catch {
      case e: java.lang.IllegalArgumentException => e
    }

    try {
      ZipCode(12345, 0)
    } catch {
      case e: java.lang.IllegalArgumentException => e
    }
    // 该方法用于在类构造时对值得有效性做一次检验，然后使用者不要再次做检验
    // 价值类不能使用断言验证
  }
}; {
  // # 调用父类构造器
  case class Address(street: String, city: String, state: String, zip: String)

  case class Person(
                     name: String,
                     age: Option[Int] = None,
                     address: Option[Address] = None)

  class Employee(
                  name: String,
                  age: Option[Int] = None,
                  address: Option[Address] = None,
                  val title: String = "[unknown]",
                  val manager: Option[Employee] = None)
  // 使用拓展父类
  // 子类若不是case类，新添加的字段要添加 val 或者 var
  // scala 覆盖方法中可以使用 super 调用父类方法，但是构造器不行
    extends Person(name, age, address) {

    // 覆盖的方法要使用 override，否则会不使用虚方法
    override def toString =
      s"Employee($name, $age, $address, $title, $manager)"
  }

  val a1 = new Address("1 Scala Lane", "Anytown", "CA", "98765")
  println(a1)
  val ceo = new Employee("Joe CEO", title = "CEO")
  println(ceo)

  println(new Employee("Buck Trends1"))
  println(new Employee("Buck Trends2", Some(20), Some(a1)))
  println(new Employee("Buck Trends3", Some(20), Some(a1), "Zombie Dev"))
  println(new Employee("Buck Trends4", Some(20), Some(a1), "Zombie Dev", Some(ceo))) {
    // ## 良好的面向对象设计：题外话
    // case 类可以派生出非 case 类，但是不能派生 case 类
    // 由于没有覆写 equals 和 hashCode 方法，可能会导致不应该的相等
    // 状态属性不应该继承
    // 如今，好的面向对象设计更倾向于组合，而不是继承
    /*
    使用继承时，建议遵循以下规则：
      - 一个抽象的基类或者 trait，只被下一层具体的类继承，包括 case 类
      - 具体类的子类永远不会再次被继承，除了 —— 类中混入了定义于 trait 的其他行为，只用于支持自动化单元测试的类
      - 当子类继承似乎是正确做法时，考虑将行为分离到 trait 中，然后在类里混入这些 trait
      - 切勿将状态逻辑跨越父类和子类
     */
  }
}; {
  // # 嵌套类型
  // scala 允许嵌套类型的成名和定义
  object Database {

    case class ResultSet(/*...*/)

    case class Connection(/*...*/)

    case class DatabaseException(message: String, cause: Throwable) extends
      RuntimeException(message, cause)

    // 使用 sealed 的继承结构表示状态，所有允许的值都在这里定义
    // 当实例实际上不携带状态信息时，使用 case 对象，表现的像标志位，表示状态
    sealed trait Status

    case object Disconnected extends Status

    case class Connected(connection: Connection) extends Status

    case class QuerySucceeded(results: ResultSet) extends Status

    case class QueryFailed(e: DatabaseException) extends Status

  }

  class Database {

    import Database._

    // ??? 是定义在 Predef 中的真实方法，调用则会抛出异常，表示方法没有被实现
    // 为编译通过实现使用
    def connect(server: String): Status = ???

    def disconnect(): Status = ???

    def query(/*...*/): Status = ???
  }
  // 嵌套类的字段被 case 的 hashCode 和 equals 忽略了
  // 需要更强的 hashCode 时，不要使用 case
}