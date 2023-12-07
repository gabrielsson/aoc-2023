package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day7Suite extends AnyFlatSpec with Matchers {
  val day = new Day7

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 6440
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 250453939
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 5905
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 248652697
  }

  it should "test card strength" in {

    val c1 = Map(
      'A' -> 1,


      )

    day.cardStrength(c1, 4) shouldBe 7
    val c2 = Map(
      '1' -> 1,
      '2' -> 1


    )
    day.cardStrength(c2, 3) shouldBe 6
  }
}
