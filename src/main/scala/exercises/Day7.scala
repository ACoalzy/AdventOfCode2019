package exercises

import intcode._
import util.DayN

object Day7 extends DayN {
  override val num = 7

  def intCodeRunner(state: State): State = {
    @annotation.tailrec
    def loop(state: State): State = {
      val opcode = state.ints(state.index)
      manageOp(opcode)(state) match {
        case Some(s) if s.finished => s
        case Some(s) if s.output.nonEmpty => s
        case Some(s) => loop(s)
      }
    }

    loop(state)
  }

  private def manageOp(code: Int)(state: State): Option[State] = {
    val op = code % 100
    val modes: IndexedSeq[ParameterMode] = code.toString.reverse.drop(2).map {
      case '0' => PositionMode
      case _ => ImmediateMode
    }

    val commanded = op match {
      case 99 => Some(Finish(state))
      case 1 => Some(Add(state, modes))
      case 2 => Some(Multiply(state, modes))
      case 3 => Some(Input(state, modes))
      case 4 => Some(Output(state, modes))
      case 5 => Some(JumpIfTrue(state, modes))
      case 6 => Some(JumpIfFalse(state, modes))
      case 7 => Some(LessThan(state, modes))
      case 8 => Some(Equals(state, modes))
    }

    commanded.map(_.run())
  }

  def maxOutput(ints: Vector[Int], phases: List[Int])(f: (Vector[Int], List[Int]) => Int) = {
    phases.permutations.map(l => f(ints, l)).max
  }

  protected def chainAmplifiersOnce(ints: Vector[Int], phases: List[Int]): Int = {
    phases.foldLeft(0){ case (in, phase) =>
      intCodeRunner(State(0, ints, List(phase, in))).output.head
    }
  }

  protected def chainAmplifiers(ints: Vector[Int], phases: List[Int]): Int = {
    val states = phases.map(p => State(0, ints, List(p)))
    def loop(input: Int, states: List[State]): Int = states match {
      case h :: t =>
        val next = intCodeRunner(h.copy(input = h.input :+ input, output = Nil))
        if (next.finished) input
        else loop(next.output.head, t :+ next)
    }

    loop(0, states)
  }

  val input: Vector[Int] = lines.head.split(",").map(_.toInt).toVector

  part1(maxOutput(input, List(0,1,2,3,4))(chainAmplifiersOnce))
  part2(maxOutput(input, List(9,8,7,6,5))(chainAmplifiers))
}
