package exercises

import intcode.{Finished, IntCode, State}
import util.{DayN, Point}

import scala.collection.immutable.Queue

object Day13 extends DayN {
  override val num = 13

  case class Game(state: State, board: Map[Point, Tile])

  sealed trait Tile { def c: Char }
  case object Empty extends Tile { val c = ' ' }
  case object Wall extends Tile { val c = '#' }
  case object Block extends Tile { val c = '-' }
  case object Paddle extends Tile { val c = '=' }
  case object Ball extends Tile { val c = '.' }
  case class Score(score: Long) extends Tile { val c = 's' }
  object Tile {
    def parse(i: Long): Tile = i match {
      case 0 => Empty
      case 1 => Wall
      case 2 => Block
      case 3 => Paddle
      case 4 => Ball
    }
  }

  def step(game: Game): Game = {
    val newState = IntCode.run(game.state)
    val pairs = newState.output.toList.grouped(3).map(parseTile)
    Game(newState, game.board ++ pairs)
  }

  def playGame(intCode: Map[Long, Long]): Game = {
    @annotation.tailrec
    def loop(game: Game): Game = {
      val tiles = game.board.toList.map(_.swap).groupBy(_._1)
      val blocks = tiles.getOrElse(Block, Nil).size
      if (game.state.status == Finished || blocks == 0) game
      else {
        val diff = tiles(Ball).head._2 - tiles(Paddle).head._2
        val dir = if (diff.x > 0) 1L else if (diff.x < 0) -1L else 0L
        val newState = game.state.copy(input = game.state.input.enqueue(dir), output = Queue())
        loop(step(game.copy(state = newState)))
      }
    }

    loop(step(Game(State(0, intCode, Queue()), Map())))
  }

  private def parseTile(ls: List[Long]): (Point, Tile) = {
    val x = ls(0)
    val y = ls(1)
    val t = ls(2)
    val tile = if (x == -1) Score(t) else Tile.parse(t)
    Point(x.toInt, y.toInt) -> tile
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)
  val free: Map[Long, Long] = input + (0L -> 2L)

  part1(playGame(input).board.values.count(_ == Block))
  part2(playGame(free).board(Point(-1, 0)))
}
