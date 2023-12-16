package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day16Suite extends AnyFlatSpec with Matchers {
  val day = new Day16

  it should "part1Test" in {
    day.part1(day.getTestRaw) shouldBe 46
  }
  it should "part1" in {
    day.part1(day.getRaw) shouldBe 7951
  }
  it should "part2Test" in {
    day.part2(day.getTestRaw) shouldBe 51
  }
  it should "part2" in {
    day.part2(day.getRaw) shouldBe 8148
  }
}
