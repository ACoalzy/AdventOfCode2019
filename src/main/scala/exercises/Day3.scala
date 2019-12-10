package exercises

import util.{DayN, Direction, Point}

object Day3 extends DayN {
  override val num = 3

  private val centralPort = Point(0, 0)

  case class Instruction(direction: Direction, distance: Int) {
    lazy val asPoint = direction.mutation * distance
  }

  def navigatePath(start: Point)(instructions: List[Instruction]): List[Point] =
    instructions.foldLeft((start, List.empty[Point])) { case ((p, acc), instruction) =>
      val (stop, points) = instructionToPoints(p, instruction)
      (stop, acc ::: points)
    }._2

  def closestIntersectionDistance(linePoints: List[List[Point]]): Option[Int] = {
    val intersections = overlaps(linePoints.map(_.toSet)).map(_.manHattanDist(centralPort))
    if (intersections.isEmpty) None else Some(intersections.min)
  }

  def earliestIntersection(linePoints: List[List[Point]]): Option[Int] = {
    val linePointDistanceMap = linePoints.map(_.zipWithIndex.map(pd => (pd._1, pd._2 + 1)).reverse.toMap)
    val intersectionLocations = overlaps(linePointDistanceMap.map(_.keySet))
    val summedIntersectionLocations = intersectionLocations.map(p => linePointDistanceMap.map(_(p)).sum)
    if (summedIntersectionLocations.isEmpty) None else Some(summedIntersectionLocations.min)
  }

  private def parseInstruction(s: String) = Instruction(Direction.fromChar(s.head), s.drop(1).toInt)

  private def instructionToPoints(start: Point, ins: Instruction): (Point, List[Point]) = {
    val destination = start + ins.asPoint
    val range = start.to(destination, ins.direction)
    (destination, range)
  }

  private def overlaps[A](sets: List[Set[A]]): Set[A] =
    sets.headOption.map(_.filter(p => sets.forall(_.contains(p)))).getOrElse(Set())

  private val lineInstructions = lines.map(_.split(",")).map(_.map(parseInstruction).toList)
  private val linePoints = lineInstructions.map(navigatePath(centralPort))
  part1(closestIntersectionDistance(linePoints))
  part2(earliestIntersection(linePoints))

}
