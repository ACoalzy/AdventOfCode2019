package intcode

sealed trait Command {
  def size: Long
  protected def modes: Seq[ParameterMode]
  protected def s: State

  protected def newIndex(i : Long): Long = i + size

  protected def newInts(i: Long, ints: Map[Long, Long]): Map[Long, Long] = ints

  protected def m(i: Int) = modes.lift(i).getOrElse(PositionMode)

  def run(): State = s.copy(index = newIndex(s.index), ints = newInts(s.index, s.ints))
}

case class Finish(s: State) extends Command {
  override def size: Long = 0L

  override protected def modes: Seq[ParameterMode] = Nil

  override def run(): State = s.copy(index = newIndex(s.index), ints = newInts(s.index, s.ints), finished = true)
}

sealed trait MathsCommand extends Command {
  val size = 4L

  protected def op: (Long, Long) => Long

  override protected def newInts(i: Long, ints: Map[Long, Long]): Map[Long, Long] = {
    val v1 = m(0).value(ints)(i + 1)
    val v2 = m(1).value(ints)(i + 2)
    val v3 = m(2).index(ints)(i + 3)
    val result = ints + (v3 -> op(v1, v2))
    result
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
  private val (input, remaining) = s.input.dequeue

  override protected def newInts(i: Long, ints: Map[Long, Long]): Map[Long, Long] = {
    ints + (m(0).index(ints)(i + 1) -> input)
  }

  override def run(): State =
    s.copy(index = newIndex(s.index), ints = newInts(s.index, s.ints), input = remaining)
}

case class Output(s: State, modes: Seq[ParameterMode] = Seq()) extends Command {
  val size = 2L

  override def run(): State = {
    val value = m(0).value(s.ints)(s.index + 1)
    s.copy(index = newIndex(s.index), ints = newInts(s.index, s.ints), output = s.output.enqueue(value))
  }
}

sealed trait JumpIf extends Command {
  val size = 3L

  protected def jump(i: Long)(condition: Long => Boolean): Long = {
    val left = m(0).value(s.ints)(i + 1)
    lazy val right = m(1).value(s.ints)(i + 2)

    if (condition(left)) right else i + size
  }
}

case class JumpIfTrue(s: State, modes: Seq[ParameterMode] = Seq()) extends JumpIf {
  override protected def newIndex(i :Long): Long = jump(i)(_ != 0)
}

case class JumpIfFalse(s: State, modes: Seq[ParameterMode] = Seq()) extends JumpIf {
  override protected def newIndex(i :Long): Long = jump(i)(_ == 0)
}

case class AdjustRelativeOffset(s: State, modes: Seq[ParameterMode] = Seq()) extends Command {
  override def size: Long = 2L

  override def run(): State = {
    val newBase = s.relativeBase + m(0).value(s.ints)(s.index + 1)
    s.copy(index = newIndex(s.index), relativeBase = newBase)
  }
}
