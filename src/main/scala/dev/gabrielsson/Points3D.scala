package dev.gabrielsson

object Points3D {

  case class Point3d(x: Int, y: Int, z: Int) {

    def +(p: Point3d): Point3d = Point3d(x + p.x, y + p.y, z + p.z)

    def -(p: Point3d): Point3d = Point3d(x - p.y, y - p.y, z - p.z)

    def *(n: Int): Point3d = Point3d(x * n, y * n, z * n)

    def neighbors: List[Point3d] =
      List(Point3d(x, y - 1, z), Point3d(x + 1, y, z), Point3d(x, y + 1, z), Point3d(x - 1, y, z), Point3d(x, y, z - 1), Point3d(x, y, z + 1))

    def corners: List[Point3d] =
      List(Point3d(x - 1, y - 1, z - 1), Point3d(x + 1, y - 1, z - 1), Point3d(x - 1, y + 1, z - 1), Point3d(x + 1, y + 1, z - 1),
        Point3d(x - 1, y - 1, z + 1), Point3d(x + 1, y - 1, z + 1), Point3d(x - 1, y + 1, z + 1), Point3d(x + 1, y + 1, z + 1))

    def surroundings: List[Point3d] = {

      for {
        x <- (x-1 to x+1).toList
        y <- (y-1 to y+1).toList
        z <- (z-1 to z+1).toList
      } yield Point3d(x, y, z)
    }.filter(p => p!=this)

    def manhattan(p: Point3d): Int = (p.x - x).abs + (p.y - y).abs + (p.z - z).abs

  }


  object Position {
    def zero: Point3d = Point3d(0, 0, 0)


  }

}