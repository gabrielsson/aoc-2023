package dev.gabrielsson

import dev.gabrielsson.GridExtensions.GridCharSeq

class Day11 extends Inputs {

  def part(input: Seq[String], expandingCoefficient: Long): Long = {
    val rowStuff = input.zipWithIndex.filter(si => si._1.forall(_ == '.')).map(_._2)
    val columnStuff = (0 until input.head.length).filter(x => {
      input.forall(s => s(x) == '.')
    })
    val grid = input.mkString("\n").toList.toGrid

    val allPlanets = grid.filter(_._2 == '#').keys
    val scoreMap = allPlanets
      .flatMap(i => {
        allPlanets.map(j => {
          val dist = (i manhattan j).toLong

          val miny = Math.min(i.y, j.y)
          val maxy = Math.max(i.y, j.y)
          val minx = Math.min(i.x, j.x)
          val maxx = Math.max(i.x, j.x)
          val additionalPointsy = rowStuff.map(idx => {

            if ((miny to maxy).contains(idx)) {
              expandingCoefficient-1L
            } else {
              0L
            }
          }).sum

          val additionalPointsx = columnStuff.map(x => {
            if ((minx to maxx).contains(x)) {
              expandingCoefficient-1L
            } else {
              0L
            }
          }).sum

          val key = List(i, j).sortBy(p => (p.x, p.y))
          val value = dist + additionalPointsy + additionalPointsx
          key -> value
        })
      })

    scoreMap.toMap.values.sum
  }
}
