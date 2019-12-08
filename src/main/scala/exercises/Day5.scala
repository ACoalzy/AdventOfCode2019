package exercises

import intcode._
import util.DayN

import scala.collection.immutable.Queue

object Day5 extends DayN {
  override val num = 5

  def intCodeRunner(ints: Vector[Int], inputs: Queue[Int]): State = {
    @annotation.tailrec
    def loop(state: State): State = {
      IntCode.step(state) match {
        case s if s.finished => state
        case s => loop(s)
      }
    }

    loop(State(0, ints, inputs))
  }

  val input: Vector[Int] = lines.head.split(",").map(_.toInt).toVector

  part1(intCodeRunner(input, Queue(1)).output.last)
  part2(intCodeRunner(input, Queue(5)).output.last)
}
