package util

case class Point(x: Int, y: Int) {
  private def mutate(op: (Int, Int) => Int)(p: Point): Point = Point(op(x, p.x), op(y, p.y))
  def +(p: Point): Point = mutate(_ + _)(p)
  def -(p: Point): Point = mutate(_ - _)(p)
  def *(p: Point): Point = mutate(_ * _)(p)
  def *(i: Int): Point = mutate(_ * _)(Point(i, i))
  def rotate(dir: Long) = if (dir == 1L) Point(y, -x) else Point(-y, x)
  def manHattanDist(p: Point): Int = math.abs(x - p.x) + math.abs(y - p.y)
  def to(target: Point, direction: Direction): List[Point] = {
    @annotation.tailrec
    def loop(curr: Point, acc: List[Point]): List[Point] = {
      curr match {
        case `target` => acc
        case p =>
          val next = curr + direction.mutation
          loop(next, next :: acc)
      }
    }

    loop(this, Nil).reverse
  }
}
