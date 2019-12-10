package exercises

import util.DayN

import scala.collection.immutable.Queue

object Day6 extends DayN {
  override val num = 6

  def countOrbits(map: Map[String, List[String]]) = {
    def navigate(k: String, depth: Int, acc: Int): Int =
      orbitMap.getOrElse(k, Nil).map(s => navigate(s, depth + 1, acc)).sum + depth

    navigate("COM", 0, 0)
  }

  def stepsToSanta(orbitMap: Map[String, List[String]], orbittingMap: Map[String, String]) = {
    case class SearchStep(s: String, depth: Int, history: Set[String])
    @annotation.tailrec
    def bfs(target: String, remaining: Queue[SearchStep]): Int = {
      remaining.dequeueOption match {
        case Some((v, q)) if v.s == target => v.depth
        case Some((v, q)) =>
          val orbits = orbitMap.getOrElse(v.s, Nil)
          val nexts = orbittingMap.get(v.s).map(_ :: orbits).getOrElse(orbits).toSet.diff(v.history)
          val nextSteps = nexts.map(s => SearchStep(s, v.depth + 1, v.history + s))
          bfs(target, q ++ nextSteps)
        case None => 0
      }
    }

    val start = orbittingMap("YOU")
    val finish = orbittingMap("SAN")
    bfs(finish, Queue(SearchStep(start, 0, Set(start))))
  }

  val orbits = lines.map(_.split("\\)")).map(a => (a(0), a(1)))
  val orbitMap = orbits.groupBy(_._1).mapValues(_.map(_._2))
  val orbittingMap = orbits.map(a => a._2 -> a._1).toMap

  part1(countOrbits(orbitMap))
  part2(stepsToSanta(orbitMap, orbittingMap))

}
