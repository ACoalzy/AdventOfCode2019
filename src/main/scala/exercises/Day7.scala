package exercises

import intcode._
import util.DayN

import scala.collection.immutable.Queue

object Day7 extends DayN {
  override val num = 7

  def intCodeRunner(state: State): State = {
    @annotation.tailrec
    def loop(state: State): State = {
      IntCode.step(state) match {
        case s if s.finished => s
        case s if state.output.size < s.output.size => s
        case s => loop(s)
      }
    }

    loop(state)
  }

  def maxOutput(ints: Vector[Int], phases: List[Int])(f: (Vector[Int], List[Int]) => Int) = {
    phases.permutations.map(l => f(ints, l)).max
  }

  protected def chainAmplifiersOnce(ints: Vector[Int], phases: List[Int]): Int = {
    phases.foldLeft(0) { case (in, phase) =>
      intCodeRunner(State(0, ints, Queue(phase, in))).output.head
    }
  }

  protected def chainAmplifiers(ints: Vector[Int], phases: List[Int]): Int = {
    val states = phases.map(p => State(0, ints, Queue(p)))

    def loop(input: Int, states: List[State]): Int = states match {
      case h :: t =>
        val next = intCodeRunner(h.copy(input = h.input.enqueue(input)))
        if (next.finished) input
        else loop(next.output.last, t :+ next)
      case _ => 0
    }

    loop(0, states)
  }

  val input: Vector[Int] = lines.head.split(",").map(_.toInt).toVector

  part1(maxOutput(input, List(0, 1, 2, 3, 4))(chainAmplifiersOnce))
  part2(maxOutput(input, List(9, 8, 7, 6, 5))(chainAmplifiers))
}
