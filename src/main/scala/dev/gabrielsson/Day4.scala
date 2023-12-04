package dev.gabrielsson

import scala.math.pow

class Day4 extends Inputs {

  def part1(input: Seq[String]): Double = {
    input.map(Card.apply)
      .map(_.points())
      .sum
  }

  def part2(input: Seq[String]): Any = {
    val cards = input.map(Card.apply)
    val cardMap = cards.map(c => c.id -> c).toMap
    val processed = cards.flatMap(c =>
      processCard(c, cardMap)
    )
    (cards ++ processed).size
  }

  private def processCard(card: Card, cardMap: Map[Int, Card]): Seq[Card] = {
    val cards = card.which().map(cardMap(_))
    cards ++ cards.flatMap(c => processCard(c, cardMap))
  }


  case class Card(id: Int, n: Seq[Int], winning: Seq[Int]) {
    private def won() = {
      winning.intersect(n)
    }

    def points(): Double = {
      if (won().isEmpty) {
        0
      } else {
        pow(2, won().size - 1)
      }
    }

    def which(): List[Int] = {
      (id + 1 to won().size + id).toList
    }
  }

  private object Card {

    private val CardRegex = """Card\W+(\d+): (.*)""".stripMargin.r

    def apply(string: String): Card = string match {
      case CardRegex(id, numbers) =>
        val strings = numbers.split("\\|")
        Card(
          Integer.parseInt(id),
          strings(0).trim.split(" ")
            .map(_.trim)
            .filterNot(_.isEmpty)
            .map(i => Integer.parseInt(i.trim)),
          strings(1).trim.split(" ").filterNot(_.trim.isEmpty).map(i => Integer.parseInt(i.trim)))
    }
  }

}
