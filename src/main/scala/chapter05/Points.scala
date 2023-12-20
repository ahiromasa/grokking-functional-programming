package chapter05

object Points {
  case class Point(x: Int, y: Int)

  assert { List(1).flatMap { x => List(-2, 7).map { y => Point(x, y) } } == List(Point(1, -2), Point(1, 7)) }

  case class Point3d(x: Int, y: Int, z: Int)

  assert {
    val points = for {
      x <- List(1)
      y <- List(-2, 7)
      z <- List(3, 4)
    } yield Point3d(x, y, z)
    points == List(Point3d(1, -2, 3), Point3d(1, -2, 4), Point3d(1, 7, 3), Point3d(1, 7, 4))
  }

  def isInside(point: Point, radius: Int): Boolean = {
    radius * radius >= point.x * point.x + point.y * point.y
  }

  val radius = for {
    r <- List(-10, 0, 2).filter { _ > 0 }
    point <- List(Point(5, 2), Point(1, 1)).filter { isInside(_, r) }
  } yield s"${point} is within a radius of ${r}"
}
