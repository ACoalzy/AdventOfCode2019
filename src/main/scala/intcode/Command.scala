package intcode

sealed trait Command {
  def size: Int
  protected def modes: Seq[ParameterMode]
  protected def s: State

  protected def newIndex(i : Int): Int = i + size

  protected def newInts(i: Int, ints: Vector[Int]): Vector[Int] = ints

  protected def m(i: Int) = modes.lift(i).getOrElse(PositionMode)

  def run(): State = State(newIndex(s.index), newInts(s.index, s.ints), s.input, s.output)
}

case class Finish(s: State) extends Command {
  override def size: Int = 0

  override protected def modes: Seq[ParameterMode] = Nil

  override def run(): State = State(newIndex(s.index), newInts(s.index, s.ints), s.input, s.output, true)
}

sealed trait MathsCommand extends Command {
  val size = 4

  protected def op: (Int, Int) => Int

  override protected def newInts(i: Int, ints: Vector[Int]): Vector[Int] =
    ints.updated(m(2).index(ints)(i + 3), op(m(0).value(ints)(i + 1), m(1).value(ints)(i + 2)))
}

case class Add(s: State, modes: Seq[ParameterMode] = Seq()) extends MathsCommand {
  val op = _ + _
}

case class Multiply(s: State, modes: Seq[ParameterMode] = Seq()) extends MathsCommand {
  val op = _ * _
}

case class LessThan(s: State, modes: Seq[ParameterMode] = Seq()) extends MathsCommand {
  val op = (l, r) => if (l < r) 1 else 0
}

case class Equals(s: State, modes: Seq[ParameterMode] = Seq()) extends MathsCommand {
  val op = (l, r) => if (l == r) 1 else 0
}

case class Input(s: State, modes: Seq[ParameterMode] = Seq()) extends Command {
  val size = 2

  override protected def newInts(i: Int, ints: Vector[Int]): Vector[Int] = {
    ints.updated(m(0).index(ints)(i + 1), s.input.head)
  }

  override def run(): State =
    State(newIndex(s.index), newInts(s.index, s.ints), s.input.drop(1), s.output)
}

case class Output(s: State, modes: Seq[ParameterMode] = Seq()) extends Command {
  val size = 2

  override def run(): State = {
    val value = m(0).value(s.ints)(s.index + 1)
    State(newIndex(s.index), newInts(s.index, s.ints), s.input.drop(1), value :: s.output)
  }
}

sealed trait JumpIf extends Command {
  val size = 3

  protected def jump(i: Int)(condition: Int => Boolean): Int = {
    val left = m(0).value(s.ints)(i + 1)
    lazy val right = m(1).value(s.ints)(i + 2)

    if (condition(left)) right else i + size
  }
}

case class JumpIfTrue(s: State, modes: Seq[ParameterMode] = Seq()) extends JumpIf {
  override protected def newIndex(i :Int): Int = jump(i)(_ != 0)
}

case class JumpIfFalse(s: State, modes: Seq[ParameterMode] = Seq()) extends JumpIf {
  override protected def newIndex(i :Int): Int = jump(i)(_ == 0)
}
