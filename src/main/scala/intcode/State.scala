package intcode

import scala.collection.immutable.Queue

case class State(
  index: Long,
  ints: Map[Long, Long],
  input: Queue[Long],
  output: Queue[Long] = Queue(),
  finished: Boolean = false,
  relativeBase: Long = 0L
)
