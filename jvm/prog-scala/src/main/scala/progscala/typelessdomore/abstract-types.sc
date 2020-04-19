import java.io._
import java.nio.file.Paths

abstract class BulkReader {
  type In
  val source: In
  def read: String
}
class StringBulkReader(val source:String) extends BulkReader {
  type In = String
  def read: String = source
}
class FileBulkReader(val source: File) extends BulkReader {
  type In = File
  def read: String = {
    val in = new BufferedInputStream(new FileInputStream(source))
    val numBytes = in.available()
    val bytes = new Array[Byte](numBytes)
    in.read(bytes, 0, numBytes)
    new String(bytes)
  }
}
println(new StringBulkReader("Hello Scala!").read)
try {
  println(new FileBulkReader(
    new File("./typelessdomore/abstract-types.sc")
  ).read)
} catch {
  case e: Any => println(e.toString)
}

// 还可以给添加参数化泛型，不同之处和 rust 相同
// 关联类型同一参数只能有一次实现
// 泛型可以有多种实现