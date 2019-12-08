package intcode

import scala.collection.immutable.Queue

case class State(
  index: Int,
  ints: Vector[Int],
  input: Queue[Int],
  output: Queue[Int] = Queue(),
  finished: Boolean = false
)
