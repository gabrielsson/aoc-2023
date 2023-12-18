package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{Grid, GridCanvas, GridCharSeq}
import dev.gabrielsson.Points.{Point, Position}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable


class GraphsTests extends AnyFlatSpec with Matchers {


  it should "add" in {
    Point(0, 0) + Point(1, 2) shouldBe Point(1, 2)
  }
  it should "manhattan" in {
    Point(4, 0) manhattan Point(0, 4) shouldBe 8
  }
  it should "surrounding" in {
    Point(0, 0).surroundings should contain allOf(Point(-1, 0), Point(-1, -1), Point(-1, 1))
  }

  it should "create grid" in {
    val grid = "122\n123\n111".toList.toIntGrid
    grid.size shouldBe 9
  }

  it should "astar" in {
    val grid =
      """1991
        |2991
        |2222""".stripMargin.toList.toIntGrid
    val goal = Point(3, 2)
    val heuristicFunction: Point => Int = _ manhattan goal

    val neighborsFunction = (p: Point) => p.neighbors.filter(n => grid.contains(n))
    val costOfTransportingFromAtoB = (a: Point, b: Point) => grid(b)
    Graphs.aStar(Some(Position.zero))(p => p == goal)(costOfTransportingFromAtoB)(neighborsFunction)(heuristicFunction)._2.get._2 shouldBe 10
  }

  it should "dijkstra" in {
    val grid =
      """1991
        |2991
        |2222""".stripMargin.toList.toIntGrid
    val neighborsFunction = (p: Point) => p.neighbors.filter(n => grid.contains(n))

    val costBetweenPoints = (from: Point, to: Point) => grid(to)

    Graphs.dijkstra(Position.zero, Point(3, 2))(neighborsFunction)(costBetweenPoints)._2.get._2 shouldBe 10
  }

  it should "bfs" in {
    val grid =
      """.###
        |..##
        |#...""".stripMargin.toList.toGrid
    val neighborsFunction: Point => Iterable[Point] = {
      p => p.neighbors.filter(n => grid.contains(n)).filter(n => grid(n) == '.')
    }

    val path = Graphs.bfsQueue(Position.zero)(neighborsFunction).map(t => (t._2, t._1))
    path(0) shouldBe Point(0, 0)
    path(1) shouldBe Point(0, 1)
    path(2) shouldBe Point(1, 1)
    path(3) shouldBe Point(1, 2)
    path(4) shouldBe Point(2, 2)
    path(5) shouldBe Point(3, 2)
  }

  it should "dfs" in {
    val grid =
      """.###
        |..##
        |#...""".stripMargin.toList.toGrid
    val neighborsFunction: Point => Iterable[Point] = {
      p => p.neighbors.filter(n => grid.contains(n)).filter(n => grid(n) != '#')
    }

    val path = Graphs.dfsStack(Position.zero)(neighborsFunction).toList.reverse
    path should contain theSameElementsAs Vector(
      Point(0, 0),
      Point(0, 1),
      Point(1, 1),
      Point(1, 2),
      Point(2, 2),
      Point(3, 2))
  }

  it should "print canvas" in {
    val grid: Grid[Char] =
      """.###
        |..##
        |#...""".stripMargin.toList.toGrid


    grid.canvas(' ')(v => v).foreach(a => {
      a.foreach(print)
      print("\n")
    })
  }

  it should "print negative canvas" in {
    val grid =
      mutable.Map[Point, Char]()

    grid(Point(-1, -1)) = 'A'
    grid(Point(-1, -2)) = 'B'
    grid(Point(-1, -3)) = 'C'
    (0 to 7).map(x => Point(x, -3)).foreach(grid(_) = '-')

    grid.toMap.canvas('.')(v => v).reverse.foreach(a => {
      a.reverse.foreach(print)
      print("\n")
    })
  }
}
