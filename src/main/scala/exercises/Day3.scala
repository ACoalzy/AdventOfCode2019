package exercises

import util.DayN
import util.geometry.direction.Direction2D
import util.geometry.point.Point2D

object Day3 extends DayN {
  override val num = 3

  private val centralPort = Point2D(0, 0)

  case class Instruction(direction: Direction2D, distance: Int) {
    lazy val asPoint2D = direction.mutation * distance
  }

  def navigatePath(start: Point2D)(instructions: List[Instruction]): List[Point2D] =
    instructions.foldLeft((start, List.empty[Point2D])) { case ((p, acc), instruction) =>
      val (stop, points) = instructionToPoint2Ds(p, instruction)
      (stop, acc ::: points)
    }._2

  def closestIntersectionDistance(linePoint2Ds: List[List[Point2D]]): Option[Int] = {
    val intersections = overlaps(linePoint2Ds.map(_.toSet)).map(_.manHattanDist(centralPort))
    if (intersections.isEmpty) None else Some(intersections.min)
  }

  def earliestIntersection(linePoint2Ds: List[List[Point2D]]): Option[Int] = {
    val linePoint2DDistanceMap = linePoint2Ds.map(_.zipWithIndex.map(pd => (pd._1, pd._2 + 1)).reverse.toMap)
    val intersectionLocations = overlaps(linePoint2DDistanceMap.map(_.keySet))
    val summedIntersectionLocations = intersectionLocations.map(p => linePoint2DDistanceMap.map(_(p)).sum)
    if (summedIntersectionLocations.isEmpty) None else Some(summedIntersectionLocations.min)
  }

  private def parseInstruction(s: String) = Instruction(Direction2D.fromChar(s.head), s.drop(1).toInt)

  private def instructionToPoint2Ds(start: Point2D, ins: Instruction): (Point2D, List[Point2D]) = {
    val destination = start + ins.asPoint2D
    val range = start.to(destination, ins.direction)
    (destination, range)
  }

  private def overlaps[A](sets: List[Set[A]]): Set[A] =
    sets.headOption.map(_.filter(p => sets.forall(_.contains(p)))).getOrElse(Set())

  private val lineInstructions = lines.map(_.split(",")).map(_.map(parseInstruction).toList)
  private val linePoint2Ds = lineInstructions.map(navigatePath(centralPort))
  part1(closestIntersectionDistance(linePoint2Ds))
  part2(earliestIntersection(linePoint2Ds))

}
