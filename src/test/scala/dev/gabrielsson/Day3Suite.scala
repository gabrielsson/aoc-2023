package dev.gabrielsson

import dev.gabrielsson.GridExtensions.GridCharSeq
import dev.gabrielsson.Points.Point
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day3Suite extends AnyFlatSpec with Matchers {
  val day = new Day3

  ignore should "part1Test" in {
    day.part1(day.getTestRaw) shouldBe 4361
  }
  ignore should "part1" in {
    day.part1(day.getRaw) shouldBe 521601
  }
  ignore should "part2Test" in {
    day.part2(day.getTestRaw) shouldBe -1
  }
  ignore should "part2" in {
    day.part2(day.getRaw) shouldBe 80694070
  }

  it should "find numbers" in {
    val grid = "..123..".toList.toGrid
    val finalList = day.pointsOfNumberContainingPoint(Point(4, 0), grid)

    finalList._2 shouldBe 123
  }
}
