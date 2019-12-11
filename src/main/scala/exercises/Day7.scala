package exercises

import intcode._
import util.DayN

import scala.collection.immutable.Queue

object Day7 extends DayN {
  override val num = 7

  def maxOutput(ints: Map[Long, Long], phases: List[Long])(f: (Map[Long, Long], List[Long]) => Long) = {
    phases.permutations.map(l => f(ints, l)).max
  }

  protected def chainAmplifiersOnce(ints: Map[Long, Long], phases: List[Long]): Long = {
    phases.foldLeft(0L) { case (in, phase) =>
      IntCode.run(State(0L, ints, Queue(phase, in))).output.head
    }
  }

  protected def chainAmplifiers(ints: Map[Long, Long], phases: List[Long]): Long = {
    val states = phases.map(p => State(0L, ints, Queue(p)))

    @annotation.tailrec
    def loop(input: Long, states: Queue[State]): Long = states.dequeue match {
      case (h, _) if h.status == Finished => input
      case (h, t) =>
        val next = IntCode.run(h.copy(input = h.input.enqueue(input)))
        loop(next.output.last, t.enqueue(next))
      case _ => 0L
    }

    loop(0, Queue() ++ states)
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(maxOutput(input, List(0L, 1L, 2L, 3L, 4L))(chainAmplifiersOnce))
  part2(maxOutput(input, List(9L, 8L, 7L, 6L, 5L))(chainAmplifiers))
}
