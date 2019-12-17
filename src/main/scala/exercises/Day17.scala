package exercises

import intcode.{IntCode, State}
import util.{DayN, Direction, Down, Point, Up}

import scala.collection.immutable.Queue

object Day17 extends DayN {
  override val num = 17

  val robotValues = Set('^', 'v', '>', '<', 'X').map(_.toLong)
  val scaffoldValues = robotValues + 35L

  case class ScaffoldMap(robot: Robot, scaffolds: Set[Point])
  case class Robot(dir: Direction, loc: Point)

  def intersections(state: State): Int = {
    val view = IntCode.run(state)
    val map = parseview(view.output.toList)
    val intersections = map.scaffolds.filter(p => Direction.all.forall(d => map.scaffolds.contains(p + d.mutation)))
    intersections.map(p => p.x * p.y).sum
  }

  private def parseview(list: List[Long]): ScaffoldMap = {
    val width = list.indexOf(10)
    val lines = list.grouped(width + 1).toList
    val scaffolds = lines.map(_.zipWithIndex.filter(i => scaffoldValues.contains(i._1))).zipWithIndex
    printMap(lines)
    val map = scaffolds.flatMap { case (l, y) => l.map { case (v, x) => v -> Point(x, y) }}.groupBy(_._1)
    val robot = robotValues.flatMap(map.get).flatten.head
    ScaffoldMap(Robot(parseDir(robot._1), robot._2), map.values.flatten.map(_._2).toSet)
  }

  private def printMap(list: List[List[Long]]): Unit = {
    println(list.map(_.map(_.toChar).mkString).mkString)
  }

  private def parseDir(value: Long): Direction = value.toChar match {
    case '^' => util.Up
    case 'v' => util.Down
    case '>' => util.Left
    case '<' => util.Right
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(intersections(State(0, input, Queue())))
}
