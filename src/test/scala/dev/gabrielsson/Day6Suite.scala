package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day6Suite extends AnyFlatSpec with Matchers {
  val day = new Day6

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 288
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 1660968
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 71503
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 26499773
  }
}
