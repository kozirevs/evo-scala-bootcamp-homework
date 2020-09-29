package homework2

object Shapes {

  sealed trait Shape[S <: Shape[S]] extends Located with Bounded with Movable[S]

  sealed trait Shape2D[S <: Shape[S]] extends Shape[S] {
    def area: Double
  }

  sealed trait Shapes3D[S <: Shape[S]] extends Shape[S] {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Located {
    def position: Array[Double]
  }

  sealed trait Bounded {
    def min: Array[Double]
    def max: Array[Double]
  }

  sealed trait Movable[S <: Shape[S]] {
    def move(delta: Array[Double]): S
  }

  final case class Point2D(x: Double, y: Double) extends Shape[Point2D] {
    override def position: Array[Double] = Array(x, y)
    override def max: Array[Double] = Array(x, y)
    override def min: Array[Double] = Array(x, y)
    override def move(delta: Array[Double]): Point2D = Point2D(x + delta.head, y + delta.lift(1).get)
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D[Circle] {
    override def position: Array[Double] = Array(centerX, centerY)
    override def max: Array[Double] = Array(centerX + radius, centerY + radius)
    override def min: Array[Double] = Array(centerX - radius, centerY - radius)
    override def move(delta: Array[Double]): Circle =
      Circle(centerX + delta.head, centerY + delta.lift(1).get, radius)
    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Rectangle(centerX: Double, centerY: Double, length: Double, height: Double) extends Shape2D[Rectangle] {
    override def position: Array[Double] = Array(centerX, centerY)
    override def max: Array[Double] = Array(centerX + length / 2, centerY + height / 2)
    override def min: Array[Double] = Array(centerX - length / 2, centerY - height / 2)
    override def move(delta: Array[Double]): Rectangle =
      Rectangle(centerX + delta.head, centerY + delta.lift(1).get, length, height)
    override def area: Double = length * height
  }

  final case class Square(centerX: Double, centerY: Double, side: Double) extends Shape2D[Square] {
    override def position: Array[Double] = Array(centerX, centerY)
    override def max: Array[Double] = Array(centerX + side / 2, centerY + side / 2)
    override def min: Array[Double] = Array(centerX - side / 2, centerY - side / 2)
    override def move(delta: Array[Double]): Square =
      Square(centerX + delta.head, centerY + delta.lift(1).get, side)
    override def area: Double = Math.pow(side, 2)
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape[Point3D] {
    override def position: Array[Double] = Array(x, y, z)
    override def max: Array[Double] = Array(x, y, z)
    override def min: Array[Double] = Array(x, y, z)
    override def move(delta: Array[Double]): Point3D =
      Point3D(x + delta.head, y + delta.lift(1).get, z + delta.lift(2).get)
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shapes3D[Sphere] {
    override def position: Array[Double] = Array(centerX, centerY, centerZ)
    override def max: Array[Double] = Array(centerX + radius, centerY + radius, centerZ + radius)
    override def min: Array[Double] = Array(centerX - radius, centerY - radius, centerZ - radius)
    override def move(delta: Array[Double]): Sphere =
      Sphere(centerX + delta.head, centerY + delta.lift(1).get, centerZ + delta.lift(2).get, radius)
    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
    override def volume: Double = 4 / 3 * Math.PI * Math.pow(radius, 3)
  }

  final case class Cube(centerX: Double, centerY: Double, centerZ: Double, edge: Double) extends Shapes3D[Cube] {
    override def position: Array[Double] = Array(centerX, centerY, centerZ)
    override def max: Array[Double] = Array(centerX + edge / 2, centerY + edge / 2, centerZ + edge / 2)
    override def min: Array[Double] = Array(centerX - edge / 2, centerY - edge / 2, centerZ - edge / 2)
    override def move(delta: Array[Double]): Cube =
      Cube(centerX + delta.head, centerY + delta.lift(1).get, centerZ + delta.lift(2).get, edge)
    override def surfaceArea: Double = 6 * Math.pow(edge, 2)
    override def volume: Double = Math.pow(edge, 3)
  }

  final case class Cuboid(centerX: Double, centerY: Double, centerZ: Double,
                          length: Double, width: Double, height: Double) extends Shapes3D[Cuboid] {
    override def position: Array[Double] = Array(centerX, centerY, centerZ)
    override def max: Array[Double] = Array(centerX + length / 2, centerY + width / 2, centerZ + height / 2)
    override def min: Array[Double] = Array(centerX - length / 2, centerY - width / 2, centerZ - height / 2)
    override def move(delta: Array[Double]): Cuboid =
      Cuboid(centerX + delta.head, centerY + delta.lift(1).get, centerZ + delta.lift(2).get, length, width, height)
    override def surfaceArea: Double = 2 * (length * width + length * height + width * height)
    override def volume: Double = length * width * height
  }
}