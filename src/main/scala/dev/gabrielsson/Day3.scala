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

    val allPoints = allPointsWithSymbol.map(p => allAdjecentNumbers(p._1, grid))

    allPoints.toSet.toList
      .map((points: Set[Point]) => points.toList.map(p => grid(p)))
      .map(s => new String(s.toArray))
      .map(s => Integer.parseInt(s))
      .sum
  }

  def allAdjecentNumbers(p: Point, grid: Grid[Char]): Set[Point] = {
    val leftList = (1 to 100).takeWhile(i => grid.get(Point(p.x - i, p.y)).exists(_.isDigit))
      .map(i => Point(p.x - i, p.y))


    val rightList = (1 to 100).takeWhile(i => grid.get(Point(p.x + i, p.y)).exists(_.isDigit))
      .map(i => Point(p.x + i, p.y))


    val finalList = (leftList ++ rightList :+ p).sortBy(_.x).toSet

    finalList
  }

  def part2(input: String): Int = {
    val grid = input.toList.toGrid

    val allGearPoints = grid.filter(t => t._2 == '*')

    val allPoints = allGearPoints.map(t => {
        val pointsWithNumbers: Seq[Point] = t._1.surroundings.filter(p => grid.get(p).exists(c => c.isDigit))
        (t._1, pointsWithNumbers)
      })
      .map(gearNumbers => {
        (gearNumbers._1, gearNumbers._2.map(p => allAdjecentNumbers(p, grid)))
      })

    allPoints.toSet.toList
      .map((t: (Point, Seq[Set[Point]])) => (t._1, t._2.toSet))
      .filter(t => t._2.size == 2)
      .map(t => (t._1, t._2
        .map(_.toList.map(p => grid(p)))
        .map(chars => new String(chars.toArray))
        .map(s => Integer.parseInt(s)))
      )
      .map(t => (t._1, t._2.product))
      .map(t => t._2)
      .sum
  }
}
