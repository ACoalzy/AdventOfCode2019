package exercises

import util.DayN

object Day2 extends DayN {
  override val num = 2

  sealed trait Command {
    def run(i: Int, ints: Vector[Int]): Vector[Int]
  }
  case object Add extends Command {
    override def run(i: Int, ints: Vector[Int]): Vector[Int] =
      ints.updated(ints(i+3), ints(ints(i+1)) + ints(ints(i+2)))
  }
  case object Multiply extends Command {
    override def run(i: Int, ints: Vector[Int]): Vector[Int] =
      ints.updated(ints(i+3), ints(ints(i+1)) * ints(ints(i+2)))
  }

  def intCodeRunner(ints: Vector[Int]): Int = {
    @annotation.tailrec
    def loop(i: Int, ints: Vector[Int]): Vector[Int] = ints(i) match {
      case 99 => ints
      case 1 => loop(i+4, Add.run(i, ints))
      case 2 => loop(i+4, Multiply.run(i, ints))
    }

    loop(0, ints).head
  }

  val input: Vector[Int] = lines.head.split(",").map(_.toInt).toVector
  part1(intCodeRunner(input.updated(1, 12).updated(2, 2)))
}
