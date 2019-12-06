package exercises

import util.DayN

object Day6 extends DayN {
  override val num = 6

  def countOrbits(map: Map[String, List[String]]) = {
    def navigate(k: String, depth: Int, acc: Int): Int =
      orbitMap.getOrElse(k, Nil).map(s => navigate(s, depth + 1, acc)).sum + depth

    navigate("COM", 0, 0)
  }

  def stepsToSanta(orbitMap: Map[String, List[String]], orbittingMap: Map[String, String]) = {
    def navigate(current: String, target: String, depth: Int, history: Set[String]): Option[Int] = {
      if (current == target) Some(depth)
      else {
        val orbits = orbitMap.getOrElse(current, Nil)
        val nexts = orbittingMap.get(current).map(_ :: orbits).getOrElse(orbits).toSet.diff(history)
        nexts.flatMap(n => navigate(n, target, depth + 1, history + n)).minOption
      }
    }

    val start = orbittingMap("YOU")
    val finish = orbittingMap("SAN")
    navigate(start, finish, 0, Set(start))
  }

  val orbits = lines.map(_.split("\\)")).map(a => (a(0), a(1)))
  val orbitMap = orbits.groupBy(_._1).mapValues(_.map(_._2)).toMap
  val orbittingMap = orbits.map(a => a._2 -> a._1).toMap

  part1(countOrbits(orbitMap))
  part2(stepsToSanta(orbitMap, orbittingMap))

}
