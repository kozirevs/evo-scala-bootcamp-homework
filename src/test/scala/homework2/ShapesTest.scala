package homework2

import homework2.Shapes.{Circle, Cube, Cuboid, Point2D, Point3D, Rectangle, Sphere, Square}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ShapesTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "move" should "move correctly" in {
    Point2D(5, -7).move(Array(-3, 6)) shouldEqual Point2D(2, -1)
    Circle(2, 6, 5).move(Array(-5, 1)) shouldEqual Circle(-3, 7, 5)
    Rectangle(9, 2, 5, 2).move(Array(-2, 9)) shouldEqual Rectangle(7, 11, 5, 2)
    Square(6, -6, 8).move(Array(5, -1)) shouldEqual Square(11, -7, 8)
    Point3D(2, 6, 5).move(Array(-5, 1, -7)) shouldEqual Point3D(-3, 7, -2)
    Sphere(2, -6, 5, 4).move(Array(-5, 1, -7)) shouldEqual Sphere(-3, -5, -2, 4)
    Cube(-2, 6, -5, 7).move(Array(-5, 1, 10)) shouldEqual Cube(-7, 7, 5, 7)
    Cuboid(2, -6, 5, 7, 2, 8).move(Array(5, -1, -10)) shouldEqual Cuboid(7, -7, -5, 7, 2, 8)
  }

  "area" should "calculate correctly" in {
    val int1: Int = 7
    val int2: Int = 8
    Circle(4, 6, int1).area shouldEqual Math.PI * Math.pow(int1, 2)
    Rectangle(4, 6, int1, int2).area shouldEqual int1 * int2
    Square(4, 6, int1).area shouldEqual Math.pow(int1, 2)
  }

  "surface area" should "calculate correctly" in {
    val int1: Int = 7
    val int2: Int = 8
    val int3: Int = 9
    Sphere(1, 2, 3, int1).surfaceArea shouldEqual 4 * Math.PI * Math.pow(int1, 2)
    Cube(1, 2, 3, int1).surfaceArea shouldEqual 6 * Math.pow(int1, 2)
    Cuboid(1, 2, 3, int1, int2, int3).surfaceArea shouldEqual 2 * (int1 * int2 + int1 * int3 + int2 * int3)
  }

  "volume" should "calculate correctly" in {
    val int1: Int = 7
    val int2: Int = 8
    val int3: Int = 9
    Sphere(1, 2, 3, int1).volume shouldEqual 4 / 3 * Math.PI * Math.pow(int1, 3)
    Cube(1, 2, 3, int1).volume shouldEqual Math.pow(int1, 3)
    Cuboid(1, 2, 3, int1, int2, int3).volume shouldEqual int1 * int2 * int3
  }
}
