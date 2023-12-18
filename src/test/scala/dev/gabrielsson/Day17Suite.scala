package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day17Suite extends AnyFlatSpec with Matchers {
  val day = new Day17

  it should "part1Test" in {
    day.part1(day.getTestRaw) shouldBe 102
  }
  it should "part1" in {
    day.part1(day.getRaw) shouldBe 742
  }
  it should "part2Test" in {
    day.part2(day.getTestRaw) shouldBe 94
  }
  it should "part2" in {
    day.part2(day.getRaw) shouldBe 918
  }
}
