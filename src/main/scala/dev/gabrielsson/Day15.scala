package dev.gabrielsson

class Day15 extends Inputs {

  case class Instruction(label: String, operation: Char, focalLength: Int = 0) {
    def getBox() = hashAlgo(label.toCharArray)
  }

  def part1(input: String): Long =
    input.split(",").map(_.toCharArray)
      .map(hashAlgo).sum

  def hashAlgo(chars: Array[Char]): Int =
    chars.foldLeft(0)((acc, c) => {
      var currentValue = acc + c
      currentValue *= 17
      currentValue = currentValue % 256
      currentValue
    })

  def part2(input: String): Int =
    input.split(",")
      .map(toInstruction)
      .foldLeft(Map[Int, List[Instruction]]().withDefaultValue(List.empty))(applyInstruction).toList
      .flatMap { case (boxId, values) =>
        calculateFinalScore(boxId, values)
      }.sum

  private def applyInstruction(boxes: Map[Int, List[Instruction]], instr: Instruction) = {
    instr.operation match {
      case '-' => minusOperation(boxes, instr)
      case _ => equalsOperation(boxes, instr)
    }
  }

  private def calculateFinalScore(boxId: Int, values: List[Instruction]): List[Int] = {
    values.zipWithIndex.map { case (value, index) =>
      (boxId + 1) * (index + 1) * value.focalLength
    }
  }

  private def toInstruction(chars: String) = {
    if (chars.contains("-")) {
      val ns = chars.split("-")
      Instruction(ns(0), '-')
    } else {
      val split = chars.indexOf("=")
      Instruction(chars.substring(0, split), '=', chars.substring(split + 1).toInt)
    }
  }

  private def minusOperation(boxes: Map[Int, List[Instruction]], instr: Instruction) = {
    val n = boxes(instr.getBox()).filter(_.label != instr.label)
    boxes + (instr.getBox() -> n)
  }

  private def equalsOperation(boxes: Map[Int, List[Instruction]], instr: Instruction) = {
    val m: List[Instruction] = boxes(instr.getBox())
    if (m.map(_.label).contains(instr.label)) {
      val i = m.find(_.label == instr.label).get
      val idx = m.indexOf(i)
      val tuple = m.splitAt(idx)
      val n = (tuple._1 :+ instr) ++ tuple._2.tail
      boxes + (instr.getBox() -> n)
    } else {
      val n: List[Instruction] = m :+ instr
      val v: Map[Int, List[Instruction]] = (boxes + (instr.getBox() -> n))
      v
    }
  }
}
