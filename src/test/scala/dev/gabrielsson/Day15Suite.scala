package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day15Suite extends AnyFlatSpec with Matchers {
  val day = new Day15

  it should "part1Test" in {
    day.part1(day.getTestRaw) shouldBe 1320
  }
  it should "part1" in {
    // not 512759
    day.part1(day.getRaw) shouldBe 512797
  }
  it should "part2Test" in {
    day.part2(day.getTestRaw) shouldBe 145
  }
  it should "part2" in {
    // not 261965 to low
    day.part2(day.getRaw) shouldBe 262454
  }
}
