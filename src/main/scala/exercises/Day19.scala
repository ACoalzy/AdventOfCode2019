package exercises

import intcode.{IntCode, State}
import util.{DayN, Display, Point}

import scala.collection.immutable.Queue

object Day19 extends DayN {
  override val num = 19

  def affectedPoints(x: Int, y: Int, state: State): Int = {
    val outs = for {
      xx <- 0 until x
      yy <- 0 until y
    } yield IntCode.run(state.copy(input = Queue(xx, yy))).output.last

    outs.count(_ == 1)
  }

  def earliestSquare(state: State, size: Int): Int = {
    @annotation.tailrec
    def loop(x: Int, y: Int): Int = {
      val topLeft = IntCode.run(state.copy(input = Queue(x, y + size - 1))).output.last
      val bottomRight = IntCode.run(state.copy(input = Queue(x + size - 1, y))).output.last
      if (topLeft == 1 && bottomRight == 1) (x * 10000) + y
      else loop(if (topLeft == 1L) x else x + 1, if (bottomRight == 1L) y else y + 1)
    }

    loop(0, 0)
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)
  part1(affectedPoints(50, 50, State(0, input, Queue())))
  part2(earliestSquare(State(0, input, Queue()), 100))
}
