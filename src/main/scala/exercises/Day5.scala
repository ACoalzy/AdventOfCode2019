package exercises

import intcode._
import util.DayN

import scala.io.StdIn

object Day5 extends DayN {
  override val num = 5

  def intCodeRunner(ints: Vector[Int]): Int = {
    @annotation.tailrec
    def loop(state: State): Vector[Int] = {
      val opcode = state.ints(state.index)
      manageOp(opcode)(state) match {
        case None => state.ints
        case Some(s) => loop(s)
      }
    }

    loop(State(0, ints)).head
  }

  private def manageOp(code: Int)(state: State): Option[State] = {
    val op = code % 100
    val modes: IndexedSeq[ParameterMode] = code.toString.reverse.drop(2).map {
      case '0' => PositionMode
      case _ => ImmediateMode
    }

    val commanded = op match {
      case 99 => None
      case 1 => Some(Add(state, modes))
      case 2 => Some(Multiply(state, modes))
      case 3 => {
        println("Please input an int:")
        val v = StdIn.readInt()
        Some(Input(state, modes, v))
      }
      case 4 => Some(Output(state, modes))
      case 5 => Some(JumpIfTrue(state, modes))
      case 6 => Some(JumpIfFalse(state, modes))
      case 7 => Some(LessThan(state, modes))
      case 8 => Some(Equals(state, modes))
    }

    commanded.map(_.run())
  }

  val input: Vector[Int] = lines.head.split(",").map(_.toInt).toVector

  part1(intCodeRunner(input)) // input 1
  part2(intCodeRunner(input)) // input 5
}
