package exercises

import intcode._
import util.DayN

import scala.collection.immutable.Queue

object Day11 extends DayN {
  override val num = 11

  case class Point(x: Int, y: Int) {
    def +(p2: Point) = Point(x + p2.x, y + p2.y)

    def rotate(dir: Long) = if (dir == 1L) Point(y, -x) else Point(-y, x)
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  def robotPainter(colour: Long, state: State): Map[Point, Long] = {
    val start = Point(0, 0)
    val direction = Point(0, 1)

    @annotation.tailrec
    def loop(colour: Long, s: State, location: Point, direction: Point, hull: Map[Point, Long]): Map[Point, Long] = {
      val newState = IntCode.run(s.copy(input = s.input.enqueue(colour), output = Queue()))
      newState.status match {
        case Finished => hull
        case _ =>
          val colour = newState.output.head
          val newDirection = direction.rotate(newState.output(1))
          loop(hull.getOrElse(location, 0L), newState, location + newDirection, newDirection, hull + (location -> colour))
      }
    }

    loop(colour, state, start, direction, Map())
  }

  def displayHull(hull: Map[Point, Long]): String = {
    val minX = hull.keySet.map(_.x).min
    val maxX = hull.keySet.map(_.x).max
    val minY = hull.keySet.map(_.y).min
    val maxY = hull.keySet.map(_.y).max

    val matrix = Array.ofDim[Char](maxY - minY + 1, maxX - minX + 1)
    hull.foreach { case (p, l) => matrix(p.y - minY)(p.x - minX) = if (l == 1L) '#' else '.' }

    matrix.map(_.mkString("")).reverse.mkString("\n")
  }

  part1(robotPainter(0L, State(0, input, Queue())).size)
  part2("\n" + displayHull(robotPainter(1L, State(0, input, Queue()))))
}
