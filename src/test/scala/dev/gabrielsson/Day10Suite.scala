package dev.gabrielsson

import dev.gabrielsson.Points.Point
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day10Suite extends AnyFlatSpec with Matchers {
  val day = new Day10

  it should "part1Test" in {
    day.part1(day.getTestRaw) shouldBe -1
  }
  it should "part1" in {
    day.part1(day.getRaw) shouldBe -1
  }
  it should "part2Test" in {
    day.part2(day.getTestRaw) shouldBe -1
  }
  it should "part2" in {
    // 179 to low
    day.part2(day.getRaw) shouldBe -1
  }

  it should "connect" in {
    day.canConnect('L', '7', Point(1,0)) shouldBe true
    day.canConnect('|', '7', Point(1,0)) shouldBe false
    day.canConnect('–', '7', Point(-1,0)) shouldBe false
    day.canConnect('|', '7', Point(-1,0)) shouldBe false
    day.canConnect('F', '7', Point(-1,0)) shouldBe false
    day.canConnect('J', '7', Point(-1,0)) shouldBe false
    day.canConnect('S', '7', Point(-1,0)) shouldBe false
    day.canConnect('S', '|', Point(-1,0)) shouldBe false
    day.canConnect('S', 'F', Point(-1,0)) shouldBe true


  }
}
