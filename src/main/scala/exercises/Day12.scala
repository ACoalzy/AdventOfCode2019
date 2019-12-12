package exercises

import util.DayN

object Day12 extends DayN {
  override val num = 12

  case class Point(x: Int, y: Int, z: Int) {
    def +(p2: Point): Point = Point(x + p2.x, y + p2.y, z + p2.z)

    def *(p2: Point): Point = Point(x * p2.x, y * p2.y, z * p2.z)

    def abs: Point = Point(math.abs(x), math.abs(y), math.abs(z))

    def sum: Int = x + y + z

    def attraction(p2: Point) = {
      def compare(i: Int, i2: Int) = if (i > i2) -1 else if (i < i2) 1 else 0

      Point(compare(x, p2.x), compare(y, p2.y), compare(z, p2.z))
    }
  }

  case class Moon(location: Point, velocity: Point = Point(0, 0, 0)) {
    def gravitise(m2: Moon) = this.copy(velocity = velocity + location.attraction(m2.location))
  }

  def simulateMotion(moons: List[Moon], duration: Int): List[Moon] = {
    @annotation.tailrec
    def loop(time: Int, moons: List[Moon]): List[Moon] =
      if (time == duration) moons
      else {
        val newMoons = applyGravity(moons).map(m => m.copy(location = m.location + m.velocity))
        loop(time + 1, newMoons)
      }

    loop(0, moons)
  }

  def simulateMotionUntilRepeat(moons: List[Moon]): Int = {
    @annotation.tailrec
    def loop(time: Int, moons: List[Moon], history: Map[List[Moon], Int]): Int = history.get(moons) match {
      case Some(v) => time
      case None =>
        val newHistory = history + (moons -> time)
        val newMoons = applyGravity(moons).map(m => m.copy(location = m.location + m.velocity))
        loop(time + 1, newMoons, newHistory)
    }

    loop(0, moons, Map())
  }

  private def applyGravity(moons: List[Moon]): List[Moon] = {
    moons.map(m => moons.foldLeft(m) { case (m1, m2) => m1.gravitise(m2) })
  }

  private def parseMoons(strings: List[String]): List[Moon] = {
    def split(s: String) = s.replace("<", "").replace(">", "").split(", ")

    def extractInts(a: Array[String]) = a.map(_.split("=")(1).toInt)

    strings.map(split).map(extractInts).map(a => Moon(Point(a(0), a(1), a(2))))
  }

  private def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) {
    (a, b) => b * a / Stream.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
  }

  private def energy(ms: List[Moon]): Int = ms.map(m => m.location.abs.sum * m.velocity.abs.sum).sum

  val dummyMoons = List(Moon(Point(-1, 0, 2)), Moon(Point(2, -10, -7)), Moon(Point(4, -8, 8)), Moon(Point(3, 5, -1)))
  val moons = parseMoons(lines)

  part1(energy(simulateMotion(dummyMoons, 10)))
  val xs = simulateMotionUntilRepeat(moons.map(m => Moon(Point(m.location.x, 0, 0))))
  val ys = simulateMotionUntilRepeat(moons.map(m => Moon(Point(0, m.location.y, 0))))
  val zs = simulateMotionUntilRepeat(moons.map(m => Moon(Point(0, 0, m.location.z))))
  part2(lcm(Seq(xs, ys, zs)))
}
