package exercises

import util.DayN

import scala.collection.immutable.Queue

object Day10 extends DayN {
  override val num = 10

  case class Asteroid(point: Point, linesOfSight: Int)

  case class Point(x: Int, y: Int) {
    def to(p2: Point) = Point(p2.x - x, p2.y - y)

    def sumAbs = math.abs(x) + math.abs(y)

    def tan2 = math.atan2(x, y)
  }

  def bestMonitoringLocation(asteroids: Set[Point]): Asteroid =
    asteroids.map(a => Asteroid(a, linesOfSight(a, asteroids - a).size)).maxBy(_.linesOfSight)

  def asteroidVaporizationOrder(laserBase: Point, asteroids: Set[Point]): Array[Point] = {
    val losMap = linesOfSight(laserBase, asteroids)
    val sortedLosMap = losMap.mapValues(_.toList.sortBy(_.to(laserBase).sumAbs))
    val clockWiseLos = losMap.keySet.toList.sorted.reverse

    @annotation.tailrec
    def loop(los: Queue[List[Point]], history: List[Point]): Array[Point] = {
      if (los.isEmpty) history.reverse.toArray
      else {
        val (l, tail) = los.dequeue
        l match {
          case h :: t => loop(tail.enqueue(t), h :: history)
          case Nil => loop(tail, history)
        }
      }
    }

    loop(Queue() ++ clockWiseLos.map(sortedLosMap), List())
  }

  private def linesOfSight(base: Point, ps: Set[Point]): Map[Double, Set[Point]] =
    ps.map(p => base.to(p).tan2 -> p).groupBy(_._1).mapValues(_.map(_._2))


  val asteroids = lines.map(_.zipWithIndex).zipWithIndex.flatMap { case (rows, y) => rows.filter(_._1 == '#').map { case (_, x) => Point(x, y) } }.toSet

  val laserBase = bestMonitoringLocation(asteroids)
  part1(laserBase.linesOfSight)
  val vaporised = asteroidVaporizationOrder(laserBase.point, asteroids - laserBase.point)
  part2((vaporised(199).x * 100) + vaporised(199).y)
}
