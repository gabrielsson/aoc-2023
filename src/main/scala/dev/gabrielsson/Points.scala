package dev.gabrielsson

object Points {

  case class Point(x: Int, y: Int) {

    def +(p: Point): Point = Point(x + p.x, y + p.y)

    def -(p: Point): Point = Point(x - p.x, y - p.y)

    def *(n: Int): Point = Point(x * n, y * n)

    def neighbors: List[Point] =
      List(Point(x, y - 1), Point(x + 1, y), Point(x, y + 1), Point(x - 1, y))

    def corners: List[Point] =
      List(Point(x - 1, y - 1), Point(x + 1, y - 1), Point(x - 1, y + 1), Point(x + 1, y + 1))

    def surroundings: List[Point] = neighbors ++ corners

    def rotate(deg: Int): Point = deg % 360 match {
      case 90 | -270 => Point(y, -x)
      case 180 | -180 => Point(-x, -y)
      case -90 | 270 => Point(-y, x)
      case _ => this
    }

    def manhattan(p: Point = Position.zero): Int = (p.x - x).abs + (p.y - y).abs

    def directionTo(other: Point): Char = other match {
      case Point(x2, y2) if x2 == x && y2 < y => 'N'
      case Point(x2, y2) if x2 == x && y2 > y => 'S'
      case Point(x2, y2) if y2 == y && x2 < x => 'W'
      case Point(x2, y2) if y2 == y && x2 > x => 'E'
      case _ => throw new IllegalArgumentException("Points are equal")
    }
  }

  case class Line(p1: Point, p2: Point) {
    val (dx, dy) = ((p2.x - p1.x).sign, (p2.y - p1.y).sign)

    def points(): Seq[Point] = {
      val max = math.max((p2.x - p1.x).abs, (p2.y - p1.y).abs)
      (0 to max).map(i => Point(p1.x + dx * i, p1.y + dy * i))
    }
  }

  object Position {
    def zero: Point = Point(0, 0)

    def neighbors: List[Point] = List(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))

    def corners: List[Point] = List(Point(1, 1), Point(-1, 1), Point(1, -1), Point(-1, -1))

    def surroundings: List[Point] = neighbors ++ corners

    def directions: Map[Char, Point] = List('E', 'W', 'N', 'S').zip(neighbors).toMap
  }

  case class Dir(p: Point, dir: Char) {

    def forward(n: Int = 1): Dir = dir match {
      case 'U' | 'N' => copy(p = p.copy(y = p.y - n))
      case 'D' | 'S' => copy(p = p.copy(y = p.y + n))
      case 'L' | 'W' => copy(p = p.copy(x = p.x - n))
      case 'R' | 'E' => copy(p = p.copy(x = p.x + n))
    }

    def rotate(clockwise: Boolean = true, n: Int = 0): Dir = dir match { // turn and move
      case 'N' => if (clockwise) Dir(Point(p.x + n, p.y), 'E') else Dir(Point(p.x - n, p.y), 'W')
      case 'S' => if (clockwise) Dir(Point(p.x - n, p.y), 'W') else Dir(Point(p.x + n, p.y), 'E')
      case 'W' => if (clockwise) Dir(Point(p.x, p.y - n), 'N') else Dir(Point(p.x, p.y + n), 'S')
      case 'E' => if (clockwise) Dir(Point(p.x, p.y + n), 'S') else Dir(Point(p.x, p.y - n), 'N')
    }

    def turn(d: Char): Dir = copy(dir = d)

    def turn(degrees: Int): Dir = {
      val dirs = Seq('N', 'E', 'S', 'W')
      val deg = (degrees.abs / 90) % 360
      val i = dirs.indexOf(dir) + (if (degrees < 0) dirs.length - deg else deg)
      copy(dir = dirs(i % dirs.length))
    }

  }

  case class Box(min: Point, max: Point) {
    def iterator: Iterator[Point] = {
      for {
        x <- (min.x to max.x).iterator
        y <- (min.y to max.y).iterator
      } yield Point(x, y)
    }
  }

  case class Diamond(center: Point, manhattan: Int) {
    def iterator: Iterator[Point] = {
      val box = for {
        x <- (center.x-manhattan to center.x + manhattan).iterator
        y <- (center.y -manhattan to center.y + manhattan).iterator
      } yield Point(x, y)
      box.filter(p => p.manhattan( center) <= manhattan)
    }
  }
}