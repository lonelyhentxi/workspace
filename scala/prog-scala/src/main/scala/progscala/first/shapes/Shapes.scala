package progscala.first.shapes

// 在 scala 中，整个类主体是这个类的构造函数
// 在类前面输入 case 关键字，构造函数的每个字段都会转换成该实例的 readonly 字段
// 默认参数显而易见，参数会从左到右自动推导
// case 关键字的另一个特征是编译器自动生成很多方法，包括 String，equals，hashCode，apply 方法
// case 关键字会导致编译器生成一个伴生对象 companion object
// 任何时候，只要对象名和类名相同，并且定义在同一个文件中，就可以称为伴生对象
// apply 方法实际上是对应的工厂方法
case class Point(x: Double = 0.0, y: Double = 0.0)

abstract class Shape() {
  // 该插值字符串（interpolated string）用法和 es 相同
  def draw(f: String => Unit): Unit = f(s"draw: ${this.toString}")
}

// case 关键字会为继承的类定义好有基类实现方法的方法
case class Circle(center: Point, radius: Double) extends Shape

case class Rectangle(lowerLeft: Point, height: Double, width: Double) extends Shape

case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape

// scala 的全部导入使用 _