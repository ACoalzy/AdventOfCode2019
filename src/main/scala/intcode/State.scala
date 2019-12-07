package intcode

case class State(
  index: Int,
  ints: Vector[Int],
  input: List[Int],
  output: List[Int] = List(),
  finished: Boolean = false
)
