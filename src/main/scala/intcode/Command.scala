package intcode

sealed trait Command {
  def size: Long

  protected def modes: Seq[ParameterMode]

  protected def s: State

  protected def newIndex(i: Long): Long = i + size

  protected def newInts(i: Long, ints: Map[Long, Long]): Map[Long, Long] = ints

  private def m(i: Int) = modes.lift(i).getOrElse(PositionMode)

  protected def modeIndex(i: Int) = m(i).index(s.ints)(s.index + i + 1)

  protected def modeValue(i: Int) = m(i).value(s.ints)(s.index + i + 1)

  def run(): State = s.copy(index = newIndex(s.index), ints = newInts(s.index, s.ints))
}

case class Finish(s: State) extends Command {
  override def size: Long = 0L

  override protected def modes: Seq[ParameterMode] = Nil

  override def run(): State = s.copy(index = newIndex(s.index), ints = newInts(s.index, s.ints), status = Finished)
}

sealed trait MathsCommand extends Command {
  val size = 4L

  protected def op: (Long, Long) => Long

  override protected def newInts(i: Long, ints: Map[Long, Long]): Map[Long, Long] = {
    ints + (modeIndex(2) -> op(modeValue(0), modeValue(1)))
  }
}

case class Add(s: State, modes: Seq[ParameterMode] = Seq()) extends MathsCommand {
  val op = _ + _
}

case class Multiply(s: State, modes: Seq[ParameterMode] = Seq()) extends MathsCommand {
  val op = _ * _
}

case class LessThan(s: State, modes: Seq[ParameterMode] = Seq()) extends MathsCommand {
  val op = (l, r) => if (l < r) 1L else 0L
}

case class Equals(s: State, modes: Seq[ParameterMode] = Seq()) extends MathsCommand {
  val op = (l, r) => if (l == r) 1L else 0L
}

case class Input(s: State, modes: Seq[ParameterMode] = Seq()) extends Command {
  val size = 2L

  override protected def newInts(i: Long, ints: Map[Long, Long]): Map[Long, Long] = {
    ints + (modeIndex(0) -> s.input.dequeue._1)
  }

  override def run(): State = {
    if (s.input.nonEmpty)
      s.copy(index = newIndex(s.index), ints = newInts(s.index, s.ints), input = s.input.dequeue._2, status = Running)
    else
      s.copy(status = Waiting)
  }
}

case class Output(s: State, modes: Seq[ParameterMode] = Seq()) extends Command {
  val size = 2L

  override def run(): State = {
    s.copy(index = newIndex(s.index), ints = newInts(s.index, s.ints), output = s.output.enqueue(modeValue(0)))
  }
}

sealed trait JumpIf extends Command {
  val size = 3L

  protected def jump(i: Long)(condition: Long => Boolean): Long = {
    if (condition(modeValue(0))) modeValue(1) else i + size
  }
}

case class JumpIfTrue(s: State, modes: Seq[ParameterMode] = Seq()) extends JumpIf {
  override protected def newIndex(i: Long): Long = jump(i)(_ != 0)
}

case class JumpIfFalse(s: State, modes: Seq[ParameterMode] = Seq()) extends JumpIf {
  override protected def newIndex(i: Long): Long = jump(i)(_ == 0)
}

case class AdjustRelativeOffset(s: State, modes: Seq[ParameterMode] = Seq()) extends Command {
  override def size: Long = 2L

  override def run(): State = {
    s.copy(index = newIndex(s.index), relativeBase = s.relativeBase + modeValue(0))
  }
}
