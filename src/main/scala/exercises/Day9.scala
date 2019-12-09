package exercises

import intcode.{IntCode, State}
import util.DayN

import scala.collection.immutable.Queue

object Day9 extends DayN {
  override val num = 9

  def intCodeRunner(ints: Map[Long, Long], inputs: Queue[Long]): State = {
    @annotation.tailrec
    def loop(state: State): State = {
      IntCode.step(state) match {
        case s if s.finished => state
        case s => loop(s)
      }
    }

    loop(State(0, ints, inputs))
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(intCodeRunner(input, Queue(1L)).output.last)
  part2(intCodeRunner(input, Queue(2L)).output)

}
