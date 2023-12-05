package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day1Suite extends AnyFlatSpec with Matchers {
  val day = new Day1

  it should "part1Test" in {
    //test data corrupted for part2...
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 54450
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 281
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 54265
  }
}
