package dev.gabrielsson

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.math.Ordering.Implicits.seqOrdering

class Day7 extends Inputs {
  case class Hand(hand: Map[Char, Int], sortedHand: Seq[Int], bet: Int, numberOfJokers: Int = 0, cards: String = "")

  def cardStrength(hand: Map[Char, Int], jokers: Int = 0): Int = {
    val max = if (hand.isEmpty) jokers else hand.values.max + jokers

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

  def part1(input: Seq[String]): Int = {
    val strength = input.map(s => s.split(" "))
      .map(a => {
        val hand = mutable.Map[Char, Int]().withDefaultValue(0)
        val cards = a(0).toCharArray
        cards.foreach(c => {
          hand(c) += 1
        })

        val cardStrengths = Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').zipWithIndex.toMap


        val sortedHand: Seq[Int] = cards.map(cardStrengths).toSeq

        Hand(hand.toMap, sortedHand, a(1).toInt, 0, a(0))

      })
      .map(a => (cardStrength(a.hand), a))


    val totals = strength.sortBy(t => {
        t._2.sortedHand
    }).reverse.sortBy(_._1).zipWithIndex
      .tapEach(t => println(s"${t._1._2.cards} ${t._1._2.bet} * ${(t._2 + 1)} = ${t._1._2.bet * (t._2 + 1)}"))
      .map(t => t._1._2.bet * (t._2+1))
    totals.sum

  }

  def part2(input: Seq[String]): Int = {
    val strength = input.map(s => s.split(" "))
      .map(a => {
        val hand = mutable.Map[Char, Int]().withDefaultValue(0)
        val cards = a(0).toCharArray
        cards.foreach(c => {
          hand(c) += 1
        })

        val cardStrengths = Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2','J').zipWithIndex.toMap

        val numberOfJokers = hand('J')

        val sortedHand: Seq[Int] = cards.map(cardStrengths).toSeq

        Hand(hand.toMap, sortedHand, a(1).toInt, numberOfJokers, a(0))

      })
      .map(a =>{
        (cardStrength(a.hand.filterNot(_._1 == 'J'), a.numberOfJokers), a)
      } )


    val totals = strength.sortBy(t => {
        t._2.sortedHand
      }).reverse.sortBy(_._1).zipWithIndex
      .tapEach(t => println(s"${t._1._2.cards} ${t._1._2.bet} * ${(t._2 + 1)} = ${t._1._2.bet * (t._2 + 1)}"))
      .map(t => t._1._2.bet * (t._2 + 1))
    totals.sum

  }
}
