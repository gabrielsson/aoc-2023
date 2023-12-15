package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day13Suite extends AnyFlatSpec with Matchers {
  val day = new Day13

  it should "part1Test" in {
    day.part1(day.groups[String](day.getTestInput, s=>s.isBlank)) shouldBe 405
  }
  it should "part1" in {
    //not 36357
    day.part1(day.groups[String](day.getInput, s=>s.isBlank)) shouldBe 35360
  }
  it should "part2Test" in {
    day.part2(day.groups[String](day.getTestInput, s=>s.isBlank)) shouldBe 400
  }
  it should "part2" in {
    //28177 to low
    day.part2(day.groups[String](day.getInput, s=>s.isBlank)) shouldBe 36755
  }
}
