package progscala.typelessdomore.shapes

case class Point(x: Double = 0.0, y: Double = 0.0) {
  def shift(deltax: Double = 0.0, deltay: Double = 0.0) =
    copy(x + deltax, x + deltay)
}

abstract class Shape() {
  def draw(offset: Point = Point(0.0f,0.0f))(f: String => Unit):Unit =
    f(s"draw(offset = $offset) , ${this.toString}")
}

case class Circle(center: Point, radius: Double) extends Shape

case class Rectangle(lowerLeft: Point, height: Double, width: Double) extends Shape

case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape
