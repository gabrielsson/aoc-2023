package dev.gabrielsson

import dev.gabrielsson.Graphs.dfs
import dev.gabrielsson.GridExtensions.{Grid, GridCanvas, GridCharSeq}
import dev.gabrielsson.Points.{Dir, Point}

import scala.collection.mutable

class Day10 extends Inputs {

  def canConnect(currentPipe: Char, nextPipe: Char, point: Point): Boolean = {

    if (point == Point(0, 1)) {
      Seq('|', '7', 'F','S').contains(currentPipe) && Seq('|', 'J', 'L').contains(nextPipe)
    } else if (point == Point(0, -1)) {
      Seq('|', 'J', 'L','S').contains(currentPipe) && Seq('|', '7', 'F').contains(nextPipe)
    } else if (point == Point(1, 0)) {
      Seq('-', 'L', 'F','S').contains(currentPipe) && Seq('-', '7', 'J').contains(nextPipe)
    } else if (point == Point(-1, 0)) {
      Seq('-', 'J', '7','S').contains(currentPipe) && Seq('-', 'L', 'F').contains(nextPipe)
    } else {
      true
    }
  }

  def validNeighbors(p: Point, grid: Grid[Char]): Seq[Point] = {

    val neighbors = p.neighbors
    val currentPipe = grid(p)
    neighbors
      .filter(grid.contains)
      .filter(n => {
        val c = grid(n)
        val bool = canConnect(currentPipe, c, n-p)
        bool
      })
  }

  /**
   * | is a vertical pipe connecting north and south.
   * - is a horizontal pipe connecting east and west.
   * L is a 90-degree bend connecting north and east.
   * J is a 90-degree bend connecting north and west.
   * 7 is a 90-degree bend connecting south and west.
   * F is a 90-degree bend connecting south and east.
   *
   * @param input
   * @return
   */
  def part1(input: String): Int = {
    val grid = input.toList.toGrid
    val neighborsFunction: Point => Seq[Point] = p => validNeighbors(p, grid)


    val start = grid.find(_._2 == 'S').get
    val costBetweenPoints = (from: Point, to: Point) => from.manhattan(to)

    val res = Graphs.dijkstra(start._1, start._1)(neighborsFunction)(costBetweenPoints)


    println(res)

    res._1.values.max

  }

  def calcNextDir(nextPos: Dir, dir: Char, grid: Grid[Char]):Char = {
    val nextPipe = grid(nextPos.p)
    if(dir == 'U') {
      if(nextPipe == '|') {
        'U'
      } else if(nextPipe == '7') {
        'L'
      } else {
        'R'
      }
    } else if (dir == 'R') {
      if (nextPipe == '-') {
        'R'
      } else if (nextPipe == '7') {
        'D'
      } else {
        'U'
      }
    } else if (dir == 'L') {
      if (nextPipe == '-') {
        'L'
      } else if (nextPipe == 'L') {
        'U'
      } else {
        'D'
      }
    } else if (dir == 'D') {
      if (nextPipe == '|') {
        'D'
      } else if (nextPipe == 'L') {
        'R'
      } else {
        'L'
      }
    } else {
      println("ERROR")
      'U'
    }
  }

  def part2(input: String): Int = {
    val grid = input.toList.toGrid
    val neighborsFunction: Point => Seq[Point] = p => validNeighbors(p, grid)


    val start = grid.find(_._2 == 'S').get
    val costBetweenPoints = (from: Point, to: Point) => from.manhattan(to)

    val res = Graphs.dijkstra(start._1, Point(-1,-1))(neighborsFunction)(costBetweenPoints)
    val visited = res._1.keys.toList
    val notVisited = (grid -- visited).keys.toList


    val onlyRealGrid = grid.map(t => if(visited.contains(t._1)) {
      t
    } else {
      (t._1, ' ')
    })
    onlyRealGrid
      .canvas('.'){
        case '-' => '═'
        case '|' => '║'
        case '7' => '╗'
        case 'L' => '╚'
        case 'F' => '╔'
        case 'J' => '╝'
        case 'S' => 'S'
        case _ => ' '
      }.foreach(a => {
      a.foreach(print)
      print("\n")
    })

    val rightDots = mutable.Set[Point]()
    val leftDots = mutable.Set[Point]()

    var currentPos = Dir(start._1, 'U')
    do {

      val nextPos = currentPos.forward()

      val nextDir = calcNextDir(nextPos, currentPos.dir, onlyRealGrid)


      val rightDot = currentPos.rotate(true, 1).p
      val leftDot = currentPos.rotate(false, 1).p
      if (onlyRealGrid.contains(rightDot) && onlyRealGrid(rightDot) == '.') {
        rightDots.add(rightDot)
      }
      if (onlyRealGrid.contains(leftDot) && onlyRealGrid(leftDot) == '.') {
        leftDots.add(leftDot)
      }
      val newRightDot = nextPos.rotate(true, 1).p
      val newLeftDot = nextPos.rotate(false, 1).p
      if (onlyRealGrid.contains(newRightDot) && onlyRealGrid(newRightDot) == '.') {
        rightDots.add(newRightDot)
      }
      if (onlyRealGrid.contains(newLeftDot) && onlyRealGrid(newLeftDot) == '.') {
        leftDots.add(newLeftDot)
      }

      currentPos = nextPos.copy(dir = nextDir)


    } while (onlyRealGrid(currentPos.p)!='S')


    println()
    println()
    val allConnected:Iterable[Point] = leftDots.flatMap(p => {

      dfs(p)(p => p.surroundings.filter(onlyRealGrid.contains).filter(s => onlyRealGrid(s) == '.'))
    })

    grid.map(t => {
        if (rightDots.contains(t._1)) {
          (t._1, 'r')
        } else if (leftDots.contains(t._1)) {
          (t._1, 'l')
        } else {
          (t._1, '.')
        }
      })
      .canvas('.') {
        case '-' => '═'
        case '|' => '║'
        case '7' => '╗'
        case 'L' => '╚'
        case 'F' => '╔'
        case 'J' => '╝'
        case 'S' => 'S'
        case 'l' => 'l'
        case 'r' => 'r'

        case _ => ' '
      }.foreach(a => {
        a.foreach(print)
        print("\n")
      })




    allConnected.size
  }

  private def canMakeItOut(p: Point, notVisited: Seq[Point], grid: Grid[Char]): Unit = {

    val neighbors = p.neighbors
      .filter(grid.contains)




  }
}


/*

val visited = res._1.keys
val notVisited = (grid -- visited).keys

notVisited.count(p => {

  val north = visited.find(v => v.y < p.y)
  val south = visited.find(v => v.y > p.y)
  val west = visited.find(v => v.x < p.x)
  val east = visited.find(v => v.x > p.x)


  north.isDefined &&
    south.isDefined &&
    west.isDefined &&
    east.isDefined

})
 */