package exercises

import intcode._
import util.geometry.point.Point2D
import util.{DayN, Display}

import scala.collection.immutable.Queue

object Day11 extends DayN {
  override val num = 11

  def robotPainter(colour: Long, state: State): Map[Point2D, Long] = {
    val start = Point2D(0, 0)
    val direction = Point2D(0, 1)

    @annotation.tailrec
    def loop(colour: Long, s: State, location: Point2D, direction: Point2D, hull: Map[Point2D, Long]): Map[Point2D, Long] = {
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

  def displayHull(hull: Map[Point2D, Long]): String = {
    val minX = hull.keySet.map(_.x).min
    val maxX = hull.keySet.map(_.x).max
    val minY = hull.keySet.map(_.y).min
    val maxY = hull.keySet.map(_.y).max

    val matrix = Array.ofDim[Char](maxY - minY + 1, maxX - minX + 1)
    hull.foreach { case (p, l) => matrix(p.y - minY)(p.x - minX) = if (l == 1L) '#' else '.' }

    matrix.map(_.mkString("")).reverse.mkString("\n")
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(robotPainter(0L, State(0, input, Queue())).size)

  val result = robotPainter(1L, State(0, input, Queue()))
  part2("\n" + Display.displayMap(result)(l => if (l == 1L) '#' else '.'))
}
