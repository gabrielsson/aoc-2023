package dev.gabrielsson

import dev.gabrielsson.Points.Point

import scala.annotation.tailrec
import scala.reflect.ClassTag

object GridExtensions {
  type Grid[A] = Map[Point, A]

  implicit class GridCanvas[A: ClassTag](val grid: Grid[A]) {
    def canvas(default: A)(cf: A => A): Array[Array[A]] = {
      val (x, y) = (grid.keys.maxBy(_.x.abs).x.abs, grid.keys.maxBy(_.y.abs).y.abs)
      val canvas = Array.tabulate(y + 1, x + 1)((_, _) => default)
      for {p <- grid}
        yield canvas(p._1.y.abs)(p._1.x.abs) = cf(p._2)
      canvas
    }

    def canvasNeg(default: A)(cf: A => A): Array[Array[A]] = {
      val (x, y) = (grid.keys.maxBy(_.x.abs).x.abs, grid.keys.maxBy(_.y.abs).y.abs)
      val canvas = Array.tabulate(y + 1, x + 1)((_, _) => default)
      for {p <- grid}
        yield canvas(p._1.y.abs)(p._1.x.abs) = cf(p._2)
      canvas
    }

  }

  implicit class GridCharSeq(val input: Seq[Char]) {
    def toGrid: Grid[Char] = makeGrid(input)(identity)

    def toIntGrid: Grid[Int] = makeGrid(input)(_.asDigit)
  }

  private def makeGrid[A](input: Seq[Char])(fn: (Char => A)): Grid[A] = {

    @tailrec
    def helper(xs: Seq[Char], acc: Grid[A], current: Point): Grid[A] = {
      xs match {
        case h :: t if h == '\n' => helper(t, acc, Point(0, current.y + 1))
        case h :: t => helper(t, acc.updated(current, fn(h)), Point(current.x + 1, current.y))
        case _ => acc
      }
    }

    helper(input, Map.empty, Point(0, 0))
  }
}


