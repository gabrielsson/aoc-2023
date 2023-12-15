package dev.gabrielsson

import dev.gabrielsson.Instruction.hashAlgo

class Day15 extends Inputs {

  def part1(input: String): Long =
    input
      .split(",")
      .map(Instruction.hashAlgo).sum

  def part2(input: String): Int = {
    input.split(",")
      .map(Instruction.fromString)
      .foldLeft(Map[Int, List[Instruction]]().withDefaultValue(List.empty))((boxes, instr) => instr(boxes))
      .flatMap { case (boxId, values) =>
        calculateFinalScore(boxId, values)
      }.sum
  }

  private def calculateFinalScore(boxId: Int, values: List[Instruction]): List[Int] = {
    values.zipWithIndex.map { case (value, index) =>
      (boxId + 1) * (index + 1) * value.focalLength
    }
  }
}

trait Instruction {
  def label: String

  def operation: Char

  def focalLength: Int

  def boxId: Int

  def apply(boxes: Map[Int, List[Instruction]]): Map[Int, List[Instruction]]
}

object Instruction {
  def hashAlgo(str: String): Int =
    str.foldLeft(0)((currentValue, c) => ((currentValue + c) * 17) % 256)

  def fromString(chars: String): Instruction = {
    if (chars.contains("-")) {
      val ns = chars.split("-")
      MinusInstruction(ns(0))
    } else {
      val split = chars.indexOf("=")
      EqualInstruction(chars.substring(0, split), chars.substring(split + 1).toInt)
    }
  }
}

case class EqualInstruction(label: String, focalLength: Int) extends Instruction {
  val operation: Char = '='

  override def boxId: Int = hashAlgo(label)

  override def apply(boxes: Map[Int, List[Instruction]]): Map[Int, List[Instruction]] = {
    boxes.updatedWith(boxId) {
      case Some(existingInstructions) =>
        val index = existingInstructions.indexWhere(_.label == label)
        if (index >= 0) {
          Some(existingInstructions.updated(index, this))
        } else {
          Some(existingInstructions :+ this)
        }
      case None =>
        Some(List(this))
    }
  }
}

case class MinusInstruction(label: String) extends Instruction {
  val operation: Char = '-'
  val focalLength: Int = 0

  override def boxId: Int = hashAlgo(label)

  override def apply(boxes: Map[Int, List[Instruction]]): Map[Int, List[Instruction]] = {
    boxes.updatedWith(boxId) {
      case Some(existingInstructions) => Some(existingInstructions.filter(_.label != label))
      case None => None
    }
  }
}
