package exercises

import intcode._
import util.DayN

import scala.collection.immutable.Queue

object Day5 extends DayN {
  override val num = 5

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(IntCode.run(State(0, input, Queue(1L))).output.last)
  part2(IntCode.run(State(0, input, Queue(5L))).output.last)
}
