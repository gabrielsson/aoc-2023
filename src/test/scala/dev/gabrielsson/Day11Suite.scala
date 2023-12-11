package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day11Suite extends AnyFlatSpec with Matchers {
  val day = new Day11

  it should "part1Test" in {
    day.part(day.getTestInput, 2L) shouldBe 374
  }
  it should "part1" in {
    day.part(day.getInput, 2L) shouldBe 9509330
  }
  it should "part2Test" in {
    day.part(day.getTestInput, 100L) shouldBe 8410
  }
  it should "part2" in {
    day.part(day.getInput, 1000000L) shouldBe 635832237682L
  }
}
