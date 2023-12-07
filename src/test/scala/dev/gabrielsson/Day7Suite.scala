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
    day.Hand.withJokers("AAKK9 1").handStrength() shouldBe 3
    day.Hand.withJokers("AAKJ9 2").handStrength() shouldBe 4
    day.Hand.withJokers("AAKQ9 3").handStrength() shouldBe 2
    day.Hand.withJokers("AQKT9 4").handStrength() shouldBe 1
    day.Hand.withJokers("JJAAA 4").handStrength() shouldBe 7
    day.Hand.withoutJokers("JJAAA 4").handStrength() shouldBe 5
  }
}
