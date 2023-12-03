package dev.gabrielsson

import scala.io.Source

trait Inputs {
  val fileName = s"${this.getClass.getTypeName.toLowerCase.replace("$", "").split("\\.").last}.txt"
  val testFileName = s"${this.getClass.getTypeName.toLowerCase.replace("$", "").split("\\.").last}test.txt"


  def getInput: Seq[String] = read(fileName)
  def getRaw: String = raw(fileName)
  def getTestInput: Seq[String] = read(testFileName)
  def getTestRaw: String = raw(testFileName)

  def getNonSeparatedIntegers: Seq[Int] = getInput.head.split("\\B").map(_.toInt).toSeq



  def raw(path: String): String = {
    val source = Source.fromResource(path)
    try {
      source.mkString
    } finally {
      source.close()
    }
  }
  def read(path: String): Seq[String] = {
    val source = Source.fromResource(path)
    try {
      source.getLines().toSeq
    } finally {
      source.close()
    }
  }

  def groups[B](strings: Seq[B], predicate: B=>Boolean): Seq[Seq[B]] = {
    val reduction = strings.foldLeft((Seq():Seq[Seq[B]], Seq[B]())) {
      case ((reduction, remainder), item) =>
        if (predicate.apply(item))
          (reduction :+ remainder, Seq())
        else
          (reduction, remainder :+ item)
    }
    reduction._1 :+ reduction._2
  }
}