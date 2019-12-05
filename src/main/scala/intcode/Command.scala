package intcode

import scala.io.StdIn

sealed trait Command {
  def size: Int

  def shift(i: Int) = i + size

  def apply(s: State)(modes: Seq[ParameterMode] = Seq()): State

  protected def mode(modes: Seq[ParameterMode])(i: Int) = modes.lift(i).getOrElse(PositionMode)
}

sealed trait MathsCommand extends Command {
  val size = 4

  protected def op: (Int, Int) => Int

  override def apply(s: State)(modes: Seq[ParameterMode]): State = {
    val m = mode(modes)_
    State(shift(s.index), s.threeIntOp(m(0), m(1), m(2))(op))
  }
}

case object Add extends MathsCommand {
  val op = _ + _
}

case object Multiply extends MathsCommand {
  val op = _ * _
}

case object LessThan extends MathsCommand {
  val op = (l, r) => if (l < r) 1 else 0
}

case object Equals extends MathsCommand {
  val op = (l, r) => if (l == r) 1 else 0
}

case object Input extends Command {
  val size = 2

  override def apply(s: State)(modes: Seq[ParameterMode] = Seq()): State = {
    println("Please input an int:")
    val v = StdIn.readInt()
    State(shift(s.index), s.store(v)(mode(modes)(0)))
  }
}

case object Output extends Command {
  val size = 2

  override def apply(s: State)(modes: Seq[ParameterMode] = Seq()) = {
    println(mode(modes)(0).value(s.ints)(s.index + 1))
    s.copy(index = shift(s.index))
  }
}

case object JumpIfTrue extends Command {
  val size = 3

  override def apply(s: State)(modes: Seq[ParameterMode] = Seq()): State = {
    s.copy(index = s.jumpIf(mode(modes)(0), mode(modes)(1))(_ != 0, size))
  }
}

case object JumpIfFalse extends Command {
  val size = 3

  override def apply(s: State)(modes: Seq[ParameterMode] = Seq()): State = {
    s.copy(index = s.jumpIf(mode(modes)(0), mode(modes)(1))(_ == 0, size))
  }
}