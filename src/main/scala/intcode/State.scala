package intcode

import scala.collection.immutable.Queue

case class State(
  index: Long,
  ints: Map[Long, Long],
  input: Queue[Long] = Queue(),
  output: Queue[Long] = Queue(),
  status: Status = Running,
  relativeBase: Long = 0L
)
