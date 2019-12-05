package intcode

sealed trait ParameterMode {
  def index(ints: Vector[Int])(i: Int): Int
  def value(ints: Vector[Int])(i: Int): Int = ints(index(ints)(i))
}

case object ImmediateMode extends ParameterMode {
  override def index(ints: Vector[Int])(i: Int): Int = i
}

case object PositionMode extends ParameterMode {
  override def index(ints: Vector[Int])(i: Int): Int = ints(i)
}