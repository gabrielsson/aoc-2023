package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{Grid, GridCharSeq}
import dev.gabrielsson.Points.Point

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

class Day13 extends Inputs {

  def part1(input: Seq[Seq[String]]): Int =

    input.map(seq => {
      val grid = seq.mkString("\n").toList.toGrid
      val (mirrorX, mirrorY) = findMirrors(grid)
      val x = mirrorX.map(_ + 1).getOrElse(0)
      val y = mirrorY.map(_ + 1).map(_ * 100).getOrElse(0)
      x + y
    }).sum


  def part2(input: Seq[Seq[String]]): Int =
    input.par
      .map(seq => seq.mkString("\n").toList.toGrid)
      .flatMap(original => {
        val (originalX, originalY) = findMirrors(original)
        original
          .map { case (point, char) => (point, if (char == '.') '#' else '.') }
          .map(smudged => original + smudged)
          .map(grid => findMirrors(grid, (originalX, originalY)))
          .map { case (mirrorX, mirrorY) =>
            val x = mirrorX.map(_ + 1).getOrElse(0)
            val y = mirrorY.map(_ + 1).getOrElse(0) * 100
            x + y
          }.filter(_ != 0).toSet.toSeq
      }).sum


  private def findMirrors(grid: Grid[Char], exclusions: (Option[Int], Option[Int]) = (None, None)): (Option[Int], Option[Int]) = {
    val maxX: Int = grid.keys.map(_.x).max
    val maxY: Int = grid.keys.map(_.y).max

    def isMirrorX(x: Int): Boolean =
      !exclusions._1.contains(x) && (0 to maxY).forall(y => pointOnRowEquals(grid, x, x + 1, y))

    def isMirrorY(y: Int): Boolean =
      !exclusions._2.contains(y) && (0 to maxX).forall(x => pointOnColumnEquals(grid, x, y, y + 1))

    val mirrorX = (0 until maxX).find(isMirrorX)
    val mirrorY = (0 until maxY).find(isMirrorY)

    (mirrorX, mirrorY)
  }

  private def pointOnColumnEquals(grid: Grid[Char], x: Int, y1: Int, y2: Int): Boolean = {
    val startPoint = Point(x, y1)
    val endPoint = Point(x, y2)
    pointsEquals(grid, startPoint, endPoint, p => p.copy(y = p.y - 1), p => p.copy(y = p.y + 1))
  }

  private def pointOnRowEquals(grid: Grid[Char], x1: Int, x2: Int, y: Int): Boolean = {
    val startPoint = Point(x1, y)
    val endPoint = Point(x2, y)

    pointsEquals(grid, startPoint, endPoint, p => p.copy(x = p.x - 1), p => p.copy(x = p.x + 1))
  }

  @tailrec
  private def pointsEquals(
    grid: Grid[Char],
    p1: Point,
    p2: Point,
    dec: Point => Point,
    inc: Point => Point): Boolean = (grid.contains(p1), grid.contains(p2)) match {
    case (false, _) | (_, false) => true
    case _ if grid(p1) == grid(p2) => pointsEquals(grid, dec(p1), inc(p2), dec, inc)
    case _ => false
  }

}
