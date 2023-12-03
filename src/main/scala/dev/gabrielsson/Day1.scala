package dev.gabrielsson

class Day1 extends Inputs {

  def part1(input: Seq[String]): Int = {
    input.map(s => {
      val nr = s.filter(_.isDigit)
      Integer.parseInt("" + nr.head + nr.last)
    }).sum
  }

  def part2(input: Seq[String]): Int = {
    input.map(s => {
      val minDigitMap = validNumbers.map(t => t._1 -> {
        val wordIndex = s.indexOf(t._2)
        val digitIndex = s.indexOf(t._1)

        if(wordIndex == -1 && digitIndex == -1) {
          -1
        } else if(wordIndex == -1) {
          digitIndex

        } else if (digitIndex == -1) {
          wordIndex
        } else {
          Math.min(wordIndex, digitIndex)
        }
      }).filter(_._2 > -1).toMap

      println(minDigitMap)

      val minTuple = minDigitMap.minBy(_._2)

      val maxDigitMap = validNumbers.map(t => t._1 -> Math.max(s.lastIndexOf(t._2), s.lastIndexOf(t._1+""))).toMap
      val maxTuple = maxDigitMap.maxBy(_._2)


      val no = Integer.parseInt(minTuple._1 + "" + maxTuple._1 )

      no
    }).tapEach(println).sum
  }

  val validNumbers = Seq("1"->"one", "2"->"two", "3"->"three", "4"->"four", "5"->"five", "6"->"six", "7"->"seven", "8"->"eight", "9"->"nine")
}
