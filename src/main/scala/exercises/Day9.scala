package exercises

import intcode.{IntCode, State}
import util.DayN

import scala.collection.immutable.Queue

object Day9 extends DayN {
  override val num = 9

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(IntCode.run(State(0, input, Queue(1L))).output.last)
  part2(IntCode.run(State(0, input, Queue(2L))).output)

}
