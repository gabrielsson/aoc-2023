package dev.gabrielsson

class Day2 extends Inputs {


  def part1(input: Seq[String]): Int = {
    input.map(Game.apply)
      .filter(willRun)
      .map(_.id)
      .sum
  }

  private def willRun(game: Game): Boolean = {
    val configuration = Map("red" -> 12, "green" -> 13, "blue" -> 14)

    val gameOutCome = game.turns.forall(turn => {
      val turnOutCome = turn.forall(c => {
        val i = configuration.getOrElse(c._1, 0)
        i >= c._2
      })
      println(s"turn $turn evaluated $turnOutCome")
      turnOutCome
    })

    println(s"game ${game.id} evaluated $gameOutCome")

    gameOutCome
  }

  def part2(input: Seq[String]): Long = {
    input.map(Game.apply)
      .map(power)
      .sum
  }

  private def power(game: Game): Long = {
    game.turns
      .flatMap(_.toList)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).max)
      .values
      .product
  }

  case class Game(id: Int, turns: Array[Map[String, Int]])

  private object Game {

    private val GameRegex = """Game (\d+): (.*)""".r

    def apply(string: String): Game = string match {
      case GameRegex(id, game) =>
        Game(
          Integer.parseInt(id),
          game.split(";")
            .map(_.split(",")
              .map(combination => {
                val Array(count, color) = combination.trim.split(" ")
                color.trim -> Integer.parseInt(count.trim)
              }).toMap
            ))

    }
  }
}

