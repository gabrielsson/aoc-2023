package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{Grid, GridCharSeq}
import dev.gabrielsson.Points.Point

class Day3 extends Inputs {


  def part1(input: String): Int = {
    val grid = input.toList.toGrid

    val allPointsWithSymbol = grid.filter(t => t._2.isDigit)
      .filter(t => t._1.surroundings.exists(p => {
        !grid.get(p).exists(c => c.isDigit) && grid.get(p).exists(c => c != '.')
      }))

    println("allpoints with symbol" + allPointsWithSymbol)

    val allPoints: Map[Set[Point], Int]= allPointsWithSymbol.map(p => pointsOfNumberContainingPoint(p._1, grid))

    allPoints
      .values
      .sum
  }

  /**
   * Grid of "...123..." and sending in Point(5,0)
   * would return Point(4,0) Point(5,0) Point(6,0)
   * @param p
   * @param grid
   * @return Set of points forming a number
   */
  def pointsOfNumberContainingPoint(p: Point, grid: Grid[Char]): (Set[Point], Int) = {
    val leftList = (1 to 100).takeWhile(i => grid.get(Point(p.x - i, p.y)).exists(_.isDigit))
      .map(i => Point(p.x - i, p.y))


    val rightList = (1 to 100).takeWhile(i => grid.get(Point(p.x + i, p.y)).exists(_.isDigit))
      .map(i => Point(p.x + i, p.y))


    val finalList = (leftList ++ rightList :+ p).sortBy(_.x).toSet

    (finalList,     Integer.parseInt(new String(finalList.map(grid(_)).toArray)))

  }

  def part2(input: String): Int = {
    val grid = input.toList.toGrid

    val allGearPoints = grid.filter(t => t._2 == '*')

    val allPoints: Map[Point, Map[Set[Point], Int]] = allGearPoints.map(t => {
        val pointsWithNumbers: Seq[Point] = t._1.surroundings.filter(p => grid.get(p).exists(c => c.isDigit))
        (t._1, pointsWithNumbers)
      })
      .map(gearNumbers => {
        (gearNumbers._1, gearNumbers._2.map(p => pointsOfNumberContainingPoint(p, grid)).toMap)
      })

    allPoints
      .map((t: (Point, Map[Set[Point], Int])) => (t._1, t._2.toSet))
      .filter(t => t._2.size == 2)
      .map(t => t._2.map(_._2).product)
      .sum
  }
}
