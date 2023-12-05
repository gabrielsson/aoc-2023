package dev.gabrielsson

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

class Day5 extends Inputs {

  case class AlmanacRange(destinationRangeStart: Long, sourceRangeStart: Long, rangeLength: Long)

  private def toAlmanacRange(a: Array[Long]): AlmanacRange = {
    val destinationRangeStart = a(0)
    val sourceRangeStart = a(1)
    val rangeLength = a(2)

    AlmanacRange(destinationRangeStart, sourceRangeStart, rangeLength)
  }

  def part1(input: Seq[Seq[String]]): Long = {
    val seeds = input.head.head.split(" ").tail.map(_.toLong)
    val almanacMap = almanacs(input).map(a => a.from -> a).toMap
    seeds.map(seed => findLast(seed, "seed", almanacMap)).min
  }

  @tailrec
  private def findLast(value: Long, source: String, almanacs: Map[String, Almanac]): Long = {

    if (source == "location") {
      value
    } else {
      val almanac = almanacs(source)
      val to = almanac.to
      val number = almanac.getCorrespondingNumber(value)
      findLast(number, to, almanacs)
    }
  }


  def part2(input: Seq[Seq[String]]): Long = {
    val ranges = input.head.head
      .split(" ").tail
      .map(_.toLong)
      .grouped(2)
      .toList
      .map(two => {
        val range = Range.Long.inclusive(two(0), two(0) + two(1), 1)

        println(s"${range.size} ==  ${two(1)} ${range.size == two(1)}")

        range
      })
    .filter(r => Seq(280775197L, 3229061264L, 77896732L,2748861189L, 3663093536L, 613340959L, 1075412586L, 3430371306L, 1532286286L).forall(l => !r.contains(l)))

    val almanacMap = almanacs(input).map(a => a.from -> a).toMap

    println("Builds done")

    val locations = ranges.par.map(seeds => {
      println("starting with rangeSize " + seeds.size)

      val minimumLocation = seeds.filter(l => l > 540000000).foldLeft(Long.MaxValue)((min, seed) => {
        val res = findLast(seed, "seed", almanacMap)
        if (seed % 10000000 == 0) {
          println(s"working $seeds with size ${seeds.size} = $seed and has ${seeds.end.-(seed)} left with current lowest $min")
        }
        if (res < min) {
          res
        } else {
          min
        }
      }
      )
      println("found min " + minimumLocation + " for range" + seeds + " of size " + seeds.size)
      minimumLocation
    })


    locations.min

  }

  private def almanacs(input: Seq[Seq[String]]) = {
    val mappings = input.tail

    val almanacList = mappings.map(mapping => {
      val almanac = AlmanacMapper.apply(mapping.head)

      val ranges = mapping.tail.map(s => s.split(" ").map(_.toLong))
        .map(a => toAlmanacRange(a))

      almanac.copy(ranges = ranges)


    })
    almanacList
  }

  case class Almanac(from: String, to: String, ranges: Seq[AlmanacRange]) {

    def getCorrespondingNumber(value: Long): Long = {

      ranges.find(a => a.sourceRangeStart <= value && a.sourceRangeStart + a.rangeLength -1 >= value)
        .map(range => {
          (value - range.sourceRangeStart) + range.destinationRangeStart
        }).getOrElse(value)
    }
  }


  private object AlmanacMapper {


    private val AlmanacRegex = """([a-zA-Z]+)-to-([a-zA-Z]+) map:""".r

    def apply(string: String): Almanac = string match {
      case AlmanacRegex(from, to) =>
        Almanac(
          from, to, Seq.empty)
    }
  }
}
