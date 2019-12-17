package exercises

import intcode.{IntCode, State}
import util.{DayN, Direction, Point}

import scala.collection.immutable.Queue

object Day17 extends DayN {
  override val num = 17

  val robotValues = Set('^', 'v', '>', '<', 'X').map(_.toLong)
  val scaffoldValues = robotValues + 35L

  case class ScaffoldMap(robot: Robot, scaffolds: Set[Point])

  case class Robot(dir: Direction, loc: Point)

  case class Movements(routine: List[String], functions: List[List[String]])

  def intersections(state: State): Int = {
    val view = IntCode.run(state)
    val map = parseview(view.output.toList)
    val intersections = map.scaffolds.filter(p => Direction.all.forall(d => map.scaffolds.contains(p + d.mutation)))
    intersections.map(p => p.x * p.y).sum
  }

  def dustCollection(state: State, movements: Movements): Long = {
    val inputs = movements.functions.foldLeft(movements.routine :: Nil)((b, l) => l :: b).reverse ::: List("n") :: Nil
    val inputString = inputs.map(_.mkString(",")).mkString("\n") + "\n"
    val input = inputString.toCharArray.map(_.toLong)

    IntCode.run(state.copy(ints = state.ints.updated(0, 2), input = Queue() ++ input)).output.last
  }

  // ALWAYS GOES STRAIGHT ACROSS AT INTERSECTIONS
  private def routeFinder(state: State): List[String] = {
    val view = IntCode.run(state)
    val map = parseview(view.output.toList)

    @annotation.tailrec
    def loop(dir: Point, loc: Point, count: Int, route: List[String]): List[String] = {
      lazy val forwards = loc + dir
      lazy val right = loc + dir.rotate(-1L)
      lazy val left = loc + dir.rotate(1L)
      if (map.scaffolds.contains(forwards)) loop(dir, forwards, count + 1, route)
      else if (map.scaffolds.contains(right)) loop(dir.rotate(-1L), loc, 0, List("R", count.toString) ++ route)
      else if (map.scaffolds.contains(left)) loop(dir.rotate(1L), loc, 0, List("L", count.toString) ++ route)
      else (count.toString :: route).reverse
    }

    loop(map.robot.dir.mutation, map.robot.loc, 0, Nil).drop(1)
  }

  private def parseview(list: List[Long]): ScaffoldMap = {
    val width = list.indexOf(10)
    val lines = list.grouped(width + 1).toList
    val scaffolds = for {
      (row, y) <- lines.zipWithIndex
      (elem, x) <- row.zipWithIndex
      if scaffoldValues.contains(elem)
    } yield elem -> Point(x, y)
    val map = scaffolds.groupBy(_._1)
    val robot = robotValues.flatMap(map.get).flatten.head
    ScaffoldMap(Robot(parseDir(robot._1), robot._2), map.values.flatten.map(_._2).toSet)
  }

  private def parseDir(value: Long): Direction = value.toChar match {
    case '^' => util.Up
    case 'v' => util.Down
    case '>' => util.Left
    case '<' => util.Right
  }

  private def routeToParts(route: List[String], lengthLimit: Int): List[Movements] = {
    case class MovementFunction(count: Int, steps: List[String])
    case class Step(mf: MovementFunction, remaining: List[String])

    @annotation.tailrec
    def routine(ls: List[String], r: List[String])(functions: List[List[String]]): List[String] = {
      if (ls.isEmpty) r.reverse
      else if (ls.startsWith(functions(0))) routine(ls.drop(functions(0).length), "A" :: r)(functions)
      else if (ls.startsWith(functions(1))) routine(ls.drop(functions(1).length), "B" :: r)(functions)
      else routine(ls.drop(functions(2).length), "C" :: r)(functions)
    }

    def extractSteps(size: Int, r: List[String]): Option[Step] = {
      val x = r.take(size)
      val (count, r2) = removeSubList(r, x)
      Some(Step(MovementFunction(count, x), r2)).filter(_ => x.mkString(",").length <= lengthLimit)
    }

    val range = 1 to lengthLimit
    val countLimit = lengthLimit / 2
    val res = for {
      a <- range.flatMap(i => extractSteps(i, route))
      b <- range.flatMap(i => extractSteps(i, a.remaining))
      c <- range.flatMap(i => extractSteps(i, b.remaining))
    } yield {
      if (c.remaining.isEmpty && (a.mf.count + b.mf.count + c.mf.count <= countLimit)) {
        val fs = a.mf.steps :: b.mf.steps :: c.mf.steps :: Nil
        val rt = routine(route, Nil)(fs)
        Some(Movements(rt, fs))
      } else None
    }

    res.flatten.toList
  }

  @annotation.tailrec
  def removeSubList[A](l: List[A], sublist: List[A], i: Int = 0): (Int, List[A]) = l.indexOfSlice(sublist) match {
    case -1 => (i, l)
    case index => removeSubList(l.patch(index, Nil, sublist.length), sublist, i + 1)
  }

  private def printMap(list: List[List[Long]]): Unit = {
    println(list.map(_.map(_.toChar).mkString).mkString)
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(intersections(State(0, input, Queue())))

  val route = routeFinder(State(0, input, Queue()))
  val routes = routeToParts(route, 20)
  part2(dustCollection(State(0, input, Queue()), routes.head))
}
