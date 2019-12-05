package exercises

import intcode._
import util.DayN

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
      case 1 => Some(Add)
      case 2 => Some(Multiply)
      case 3 => Some(Input)
      case 4 => Some(Output)
      case 5 => Some(JumpIfTrue)
      case 6 => Some(JumpIfFalse)
      case 7 => Some(LessThan)
      case 8 => Some(Equals)
    }

    commanded.map(_(state)(modes))
  }

  val input: Vector[Int] = lines.head.split(",").map(_.toInt).toVector
//  val input: Vector[Int] = Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)

  part1(intCodeRunner(input)) // input 1
  part2(intCodeRunner(input)) // input 5
}