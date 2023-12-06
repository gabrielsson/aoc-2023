package dev.gabrielsson

class Day6 extends Inputs {
  def part1(input: Seq[String]): Long = {
    val times = extractNumbers(input.head).map(_.toInt)
    val distances = extractNumbers(input.last).map(_.toInt)
    times.zip(distances).map { case (t, d) => countCombinations(t, d) }.product
  }

  def part2(input: Seq[String]): Long = {
    val t = extractNumbers(input.head).mkString.toLong
    val d = extractNumbers(input.last).mkString.toLong
    countCombinations(t, d)
  }

  private def extractNumbers(row: String): Iterator[String] =
    "\\d+".r.findAllIn(row)

  private def countCombinations(t: Long, d: Long): Long =
    Range.Long(0L, t, 1)
      .map(i => i * (t - i))
      .filter(_ > d)
      .map(_ => 1)
      .sum
}
