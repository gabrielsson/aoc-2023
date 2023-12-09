package dev.gabrielsson

class Day9 extends Inputs {

  def part1(input: Seq[String]): Long =
    extractListOfNumbers(input)
      .map(extrapolateNext)
      .sum

  def part2(input: Seq[String]): Long =
    extractListOfNumbers(input)
      .map(_.reverse)
      .map(extrapolateNext)
      .sum

  private final def extrapolateNext(se: Seq[Long]): Long =
    if (se.forall(_ == se.head)) {
      se.last
    } else {
      val res = se.sliding(2).map(s => {
        s(1) - s.head
      }).toSeq
      se.last + extrapolateNext(res)
    }

  private def extractListOfNumbers(input: Seq[String]): Seq[Seq[Long]] =
    input
      .map(_.split(" ")
        .map(_.toLong)
        .toSeq)
}
