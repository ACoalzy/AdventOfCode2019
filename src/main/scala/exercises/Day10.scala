package exercises

import util.DayN

import scala.collection.immutable.Queue

object Day10 extends DayN {
  override val num = 10

  case class Point(x: Int, y: Int) {
    def gcd() = {
      def gcd(a: Int, b: Int): Int = if (b==0) a.abs else gcd(b, a%b)

      val mul = gcd(math.abs(x), math.abs(y))
      Point(x / mul, y / mul)
    }

    def diff(p2: Point) = Point(p2.x - x, p2.y - y)

    def sumAbs = math.abs(x) + math.abs(y)
  }

  private val clockwiseOrdering: Ordering[Point] = new Ordering[Point] {
    override def compare(a: Point, b: Point): Int = {
      val tanA = math.atan2(a.x, a.y)
      val tanB = math.atan2(b.x, b.y)
      tanB - tanA match {
        case i if i < 0 => -1
        case i if i > 0 => 1
        case _ => 0
      }
    }
  }

  def bestMonitoringLocation(asteroids: Set[Point]): (Point, Int) =
    asteroids.map(a => a -> countVisibleFrom(a, asteroids)).maxBy(_._2)

  def countVisibleFrom(asteroid: Point, belt: Set[Point]): Int =
    linesOfSight(asteroid, belt - asteroid).size

  def asteroidVaporizationOrder(asteroids: Set[Point]) = {
    val laserBase = bestMonitoringLocation(asteroids)._1
    val losMap = linesOfSight(laserBase, asteroids - laserBase)
    val sortedLosMap = losMap.mapValues(_.toList.sortBy(_.diff(laserBase).sumAbs))
    val clockWiseLos = losMap.keySet.toList.sorted(clockwiseOrdering)

    def loop(los: Queue[Point], remainingAsteroids: Map[Point, List[Point]], history: List[Point]): List[Point] = {
      if (los.isEmpty) history.reverse
      else {
        val (l, tail) = los.dequeue
        remainingAsteroids(l) match {
          case h :: t => loop(tail.enqueue(l), remainingAsteroids.updated(l, t), h :: history)
          case Nil => loop(tail, remainingAsteroids, history)
        }
      }
    }

    loop(Queue() ++ clockWiseLos, sortedLosMap, List())
  }

  private def linesOfSight(base: Point, ps: Set[Point]): Map[Point, Set[Point]] =
    ps.map(p => base.diff(p).gcd() -> p).groupBy(_._1)
      .mapValues(_.map(_._2))

  val asteroids = lines.map(_.zipWithIndex).zipWithIndex.flatMap { case (rows, y) => rows.filter(_._1 == '#').map { case (_, x) => Point(x, y) } }.toSet

  part1(bestMonitoringLocation(asteroids))
  val vaporised200 = asteroidVaporizationOrder(asteroids)(199)
  part2((vaporised200.x * 100) + vaporised200.y)
}
