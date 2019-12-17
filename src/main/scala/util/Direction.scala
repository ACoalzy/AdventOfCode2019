package util

sealed trait Direction {
  def mutation: Point
}
object Direction {
  def fromChar(c: Char): Direction = c match {
    case 'U' => Up
    case 'D' => Down
    case 'L' => Left
    case 'R' => Right
  }
  def all: Set[Direction] = Set(Left, Right, Up, Down)
}
case object Left extends Direction { val mutation = Point(-1, 0) }
case object Right extends Direction { val mutation = Point(1, 0) }
case object Up extends Direction { val mutation = Point(0, -1) }
case object Down extends Direction { val mutation = Point(0, 1) }
