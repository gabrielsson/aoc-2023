package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{GridCanvas, GridCharSeq}
import dev.gabrielsson.Points.{Dir, Point, Position}

import scala.collection.mutable

class Day17 extends Inputs {

  def part1(input: String): Int = {
    val grid = input.toList.toIntGrid

    val maxx = grid.map(_._1.x).max
    val maxy = grid.map(_._1.y).max


    val neighbors: ((Point, DirectionCount)) => List[(Point, DirectionCount)] = t => {
      val dc = t._2
      val p = t._1
      if (dc.count > 0) {

        p.neighbors.filter(n => grid.contains(n)).map(n => {
            val newDir = p.directionTo(n)
            val newCount = if (dc.dir == newDir) {
              dc.count - 1
            } else {
              2
            }
            (n, DirectionCount(newDir, newCount))
          })
          .filter(t => {
            t._1.directionTo(p) != dc.dir
          })
      } else {
        val neighbors = p.neighbors.filter(n => grid.contains(n))

          .map(n => {
            (n, DirectionCount(p.directionTo(n), 2))
          }).filter(l => l._2.dir != dc.dir)
        neighbors
          .filter(t => {
            t._1.directionTo(p) != dc.dir
          })
      }
    }

    val costBetweenPoints = (from: (Point, DirectionCount), to: (Point, DirectionCount)) => grid(to._1)
    val heuristicFunction: ((Point, DirectionCount)) => Int = t => {
      t._1.manhattan(Position.zero)
    }


    val goalFunction: ((Point, DirectionCount)) => Boolean = t => t._1 == Point(maxx, maxy)
    val start = List((Position.zero, DirectionCount('E', 2)),(Position.zero, DirectionCount('S', 2)))
    val res = Graphs.aStar(start)(goalFunction)(costBetweenPoints)(neighbors)(heuristicFunction)

    res._2.get._2._1
  }


  case class DirectionCount(dir: Char, count: Int) {
    override def equals(obj: Any): Boolean = {
      obj match {
        case dc: DirectionCount => dir.equals(dc.dir)
        case _ => false
      }
    }
  }


  def part2(input: String): Int = {
    val grid = input.toList.toIntGrid

    val maxx = grid.map(_._1.x).max
    val maxy = grid.map(_._1.y).max


    val func: ((Point, DirectionCount)) => List[(Point, DirectionCount)] = t => {
      val dc = t._2
      val p = t._1
      if (dc.count > 6) {
        val d = Dir(p, dc.dir).forward()
        val newPoint = d.p
        val newDirectionCount = dc.copy(count = dc.count - 1)

        List((newPoint, newDirectionCount)).filter(t => grid.contains(t._1))
      } else if (dc.count > 0) {
        p.neighbors.filter(n => grid.contains(n)).map(n => {
            val newDir = p.directionTo(n)
            val newCount = if (dc.dir == newDir) {
              dc.count - 1
            } else {
              9
            }
            (n, DirectionCount(newDir, newCount))
          })
          .filter(t => {
            t._1.directionTo(p) != dc.dir
          })
      } else {
        val neighbors = p.neighbors.filter(n => grid.contains(n))
          .map(n => {
            (n, DirectionCount(p.directionTo(n), 9))
          }).filter(l => l._2.dir != dc.dir)
        neighbors
          .filter(t => {
            t._1.directionTo(p) != dc.dir
          })
      }
    }

    val costBetweenPoints = (from: (Point, DirectionCount), to: (Point, DirectionCount)) => grid(to._1)
    val heuristicFunction: ((Point, DirectionCount)) => Int = t => {
      t._1.manhattan(Position.zero)
    }


    val goalFunction: ((Point, DirectionCount)) => Boolean = t => t._1 ==  Point(maxx, maxy)
    val starts = Seq((Position.zero, DirectionCount('S', 9)),(Position.zero, DirectionCount('E', 9)))
    val res = Graphs.aStar(starts)(goalFunction)(costBetweenPoints)(func)(heuristicFunction)

    val trail = res._2.get._2._2.toSeq
    val newGrid = grid.map(t => {
        if (!trail.map(_._1).contains(t._1)) {
          (t._1, '.')
        } else t
      })
      .canvas(' ')(v => v).foreach(a => {
        a.foreach(print)
        print("\n")
      })
    res._2.get._2._1
  }
}
