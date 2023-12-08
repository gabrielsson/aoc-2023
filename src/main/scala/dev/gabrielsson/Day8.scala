package dev.gabrielsson

import scala.annotation.tailrec

class Day8 extends Inputs {
  def part1(input: Seq[String]): Int = {
    val nodeMap = toNodeMap(input)
    val nextFunc: (String, Int) => String = nextFunction(input, nodeMap)
    val start = "AAA"
    val endFunc = (node: String) => node == "ZZZ"
    traverse(start)(endFunc)(nextFunc)
  }

  def part2(input: Seq[String]): Long = {
    val nodeMap = toNodeMap(input)
    val nextFunc: (String, Int) => String = nextFunction(input, nodeMap)
    val startNodes = nodeMap.keys.filter(_.endsWith("A"))
    val results = startNodes.map(n => {
      val start = n
      val endFunc = (node: String) => node.endsWith("Z")
      traverse(start)(endFunc)(nextFunc)

    })

    results.map(_.toLong).tail
      .foldLeft(results.head.toLong)((a, b) => lcm(a, b))
  }

  private def extractNodes(row: String): Iterator[String] =
    "[A-Z]{3}".r.findAllIn(row)


  private def toNodeMap(input: Seq[String]): Map[String, (String, String)] = {
    input.tail
      .filter(_.nonEmpty) // first row is empty
      .map(s => extractNodes(s).toList) // get all three letter nodes in an array
      .map(i => i.head -> (i(1), i(2)))
      .toMap
  }

  private def nextFunction(input: Seq[String], nodeMap: Map[String, (String, String)]): (String, Int) => String = {
    val instructions = input.head.toCharArray.zipWithIndex
    val len = instructions.length

    (n: String, numberOfSteps: Int) => {
      val inst: Char = instructions.find(_._2 == numberOfSteps % len).get._1
      val t = nodeMap(n)
      if (inst == 'L') {
        t._1
      } else {
        t._2
      }
    }
  }

  @tailrec
  private def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

  private def lcm(a: Long, b: Long) = (a * b).abs / gcd(a, b)
}

object traverse {
  def apply(start: String)(endFunc: String => Boolean)(nextFunc: (String, Int) => String): Int = {
    var node = start
    var numberOfSteps = 0
    while (!endFunc(node)) {
      node = nextFunc(node, numberOfSteps)
      numberOfSteps += 1
    }
    numberOfSteps
  }
}
