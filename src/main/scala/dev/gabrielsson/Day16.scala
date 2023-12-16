package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{Grid, GridCharSeq}
import dev.gabrielsson.Points.{Dir, Point, Position}

import scala.collection.immutable.Seq
import scala.collection.parallel.CollectionConverters.IterableIsParallelizable

class Day16 extends Inputs {


  def part1(input: String): Int = {
    val grid = input.toList.toGrid
    val neighborsFunction: Dir => Seq[Dir] = (d: Dir) => {
      val n: Seq[Dir] = calculateNeighbors(d, grid)
      n.filter(d => grid.contains(d.p))
    }

    val res = Graphs.dfsStack(Dir(Position.zero, 'E'))(neighborsFunction)
    res.map(_.p).toSet.toSeq.length

  }

  def part2(input: String): Int = {
    val grid = input.toList.toGrid
    val neighborsFunction: Dir => Seq[Dir] = (d: Dir) => {
      val n: Seq[Dir] = calculateNeighbors(d, grid)
      n.filter(d => grid.contains(d.p))
    }

    startDirections(grid).par.map(start => {
      val res = Graphs.dfsStack(start)(neighborsFunction)
      res.map(_.p).toSet.toSeq.length
    }).max
  }

  private def startDirections(grid: Grid[Char]): Iterable[Dir] = {
    val allPoints = grid.keys
    val maxx = allPoints.maxBy(_.x)
    val maxy = allPoints.maxBy(_.y)

    val edges = allPoints.flatMap { p =>
      val dirs = Seq(
        if (p.x == maxx) Some(Dir(p, 'W')) else None,
        if (p.x == 0) Some(Dir(p, 'E')) else None,
        if (p.y == maxy) Some(Dir(p, 'N')) else None,
        if (p.y == 0) Some(Dir(p, 'S')) else None
      )
      dirs.flatten
    }
    edges
  }

  private def isSplit(d: Dir, c: Char): Boolean = {
    (Seq('E', 'W').contains(d.dir) && c == '|') ||
      (Seq('S', 'N').contains(d.dir) && c == '-')
  }

  private def calculateNeighbors(d: Dir, grid: Map[Point, Char]): Seq[Dir] = {
    grid(d.p) match {
      case c if isSplit(d, c) => Seq(d.rotate(false, 1), d.rotate(true, 1))
      case '/' if Seq('E', 'W').contains(d.dir) => Seq(d.rotate(false, 1))
      case '/' => Seq(d.rotate(true, 1))
      case '\\' if Seq('E', 'W').contains(d.dir) => Seq(d.rotate(true, 1))
      case '\\' => Seq(d.rotate(false, 1))
      case _ => Seq(d.forward())
    }
  }
}
