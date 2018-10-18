object Char extends Enumeration {
  type Char = Value
  val A = Value("A")
  val B = Value("B")
}
import Char._

println("ID\tChar")
for(char <- Char.values) println(s"${char.id}\t$char")
// 尽管类型名和方法名都是 Value，但是并不存在命名冲突
// 因为编译器会维护各自独立命名空间
// 无参数的 Value 方法将对象名作为输入字符串
// 第三个 Value 方法的输入参数数值作为整数 ID 值
// 否则 Value 会从 0 开始分派