package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day8Suite extends AnyFlatSpec with Matchers {
  val day = new Day8

  it should "part1Test" in {
    val input =
      """|LLR
        |
        |AAA = (BBB, BBB)
        |BBB = (AAA, ZZZ)
        |ZZZ = (ZZZ, ZZZ)
        |""".stripMargin
    day.part1(input.linesIterator.toSeq) shouldBe 6
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 17141
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 6
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 10818234074807L
  }

}
