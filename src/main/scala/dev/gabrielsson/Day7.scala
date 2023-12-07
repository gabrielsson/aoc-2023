package dev.gabrielsson

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.math.Ordering.Implicits.seqOrdering

class Day7 extends Inputs {

  def part1(input: Seq[String]): Int = {
    val hands = input.map(Hand.withoutJokers)
    val totals = winningsByHand(hands)
    totals.sum
  }

  def part2(input: Seq[String]): Int = {
    val hands = input.map(Hand.withJokers)
    val totals = winningsByHand(hands)
    totals.sum
  }

  private def winningsByHand(hands: Seq[Hand]) = {
    hands
      .sortBy(_.cardStrengths).reverse
      .sortBy(_.handStrength()).zipWithIndex
      .tapEach(t => println(s"${t._1.cards} ${t._1.bet} * ${(t._2 + 1)} = ${t._1.bet * (t._2 + 1)}"))
      .map(t => t._1.bet * (t._2 + 1))
  }

  case class Hand(hand: Map[Char, Int], cardStrengths: Seq[Int], bet: Int, numberOfJokers: Int = 0, cards: String = "") {
    def handStrength(): Int = {
      val max = hand.values.maxOption.getOrElse(0) + numberOfJokers

      max match {
        case 5 => 7
        case 4 => 6
        case 3 =>
          if (hand.count(_._2 >= 2) == 2) 5
          else 4
        case 2 =>
          if (hand.count(_._2 >= 2) == 2) 3
          else 2
        case _ => 1
      }
    }
  }

  object Hand {
    def withoutJokers(s: String): Hand = {
      val allCards = Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').zipWithIndex.toMap

      val a = s.split(" ")
      val hand = mutable.Map[Char, Int]().withDefaultValue(0)
      val cards = a(0).toCharArray
      cards.foreach(c => {
        hand(c) += 1
      })

      val cardStrengths: Seq[Int] = cards.map(allCards).toSeq

      Hand(hand.toMap, cardStrengths, a(1).toInt, 0, a(0))
    }

    def withJokers(s: String): Hand = {
      val allCards = Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').zipWithIndex.toMap

      val a = s.split(" ")
      val hand = mutable.Map[Char, Int]().withDefaultValue(0)
      val cards = a(0).toCharArray
      cards.foreach(c => {
        hand(c) += 1
      })

      val numberOfJokers = hand('J')
      val cardStrengths: Seq[Int] = cards.map(allCards).toSeq
      Hand(hand.toMap.filterNot(_._1 == 'J'), cardStrengths, a(1).toInt, numberOfJokers, a(0))
    }
  }
}
