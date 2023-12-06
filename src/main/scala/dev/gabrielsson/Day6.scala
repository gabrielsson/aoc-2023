package dev.gabrielsson

import dev.gabrielsson.Graphs.dfsStack


class Day6 extends Inputs {
  def part1(input: Seq[String]): Long = {
    val times = extractNumbers(input.head).map(_.toInt)
    val distances = extractNumbers(input.last).map(_.toInt)
    val c = times.zip(distances).map {
      case (t, d) => countCombinations(t, d)
    }
    c.product
  }

  def part2(input: Seq[String]): Long = {
    val t = extractNumbers(input.head).mkString.toLong
    val d = extractNumbers(input.last).mkString.toLong
    countCombinations(t, d)
  }

  private def extractNumbers(row: String): Iterator[String] =
    "\\d+".r.findAllIn(row)


  private def countCombinations(t: Long, d: Long): Long =
    (1L until t).count(i => i * (t - i) > d)

  private def countCombinationsDfs(t: Long, d: Long): Long = {
    def findNext(c: Long, t: Long, d: Long): Iterable[Long] = {
      var nextHoldTime = c + 1
      while (nextHoldTime < t) {
        if (nextHoldTime * (t - nextHoldTime) > d) {
          return Some(nextHoldTime)
        }
        nextHoldTime += 1
      }
      None
    }

    val neighborsFunc = findNext(_, t, d)
    dfsStack(-1L)(neighborsFunc)
      .count(_ > 0) // remove the -1 start
  }

}
