package exercises

import util.DayN
import util.geometry.edge.DirectedWeightedEdge
import util.geometry.point.Point2D
import util.search.{BFS, Dijkstra}

object Day18 extends DayN {
  override val num = 18

  case class PointAndHistory(p: Point2D, keysRemaining: Set[Char])
  case class Tunnel(start: Point2D, edgeMap: Map[Point2D, Set[DirectedWeightedEdge[Point2D]]])
  case class Input(starts: Set[Point2D], path: Set[Point2D], walls: Set[Point2D], keys: Map[Point2D, Char], doors: Map[Point2D, Char])

  def search(input: Input, tunnels: Set[Tunnel]): Set[Map[PointAndHistory, Int]] = {
    def next(tunnel: Tunnel)(pah: PointAndHistory): Set[DirectedWeightedEdge[PointAndHistory]] = {
      //Note: this treats doors locked by keys in other tunnels as open #hacky
      val isClosedDoor = input.doors.get(pah.p).map(_.toLower).exists(pah.keysRemaining.contains)
      if (isClosedDoor) Set.empty
      else {
        tunnel.edgeMap.getOrElse(pah.p, Set()).map(e =>
          e.copy(from = pah, to = PointAndHistory(e.to, if (input.doors.contains(e.to)) pah.keysRemaining else pah.keysRemaining - input.keys(e.to)))
        )
      }
    }

    val result = tunnels.map(tunnel => {
      val s = PointAndHistory(tunnel.start, tunnel.edgeMap.keySet.flatMap(input.keys.get))
      Dijkstra.searchWithExit(s)(_.keysRemaining.isEmpty)(next(tunnel))
    })
    result
  }

  def mapTunnels(input: Input): Set[Tunnel] = {
    val pointsOfInterest = input.keys.keySet ++ input.doors.keySet ++ input.starts
    val neighbourMap = input.path.map(p => p -> p.neighbours.intersect(input.path)).toMap

    def neighbours(exclude: Point2D)(p: Point2D): Set[Point2D] =
      if (p != exclude && pointsOfInterest.contains(p)) Set() else neighbourMap(p)

    def distance(exclude: Point2D)(p: Point2D, trail: Vector[Point2D]): Option[Int] =
      Some(trail.size).filter(_ => p != exclude && pointsOfInterest.contains(p))

    input.starts.map(start => Tunnel(start, {
      val accessibles = BFS.fullSearch(start)(neighbourMap)(distance(start)).keySet
      (accessibles + start).flatMap(a => BFS.fullSearch(a)(neighbours(a))(distance(a)).map { case (p2, dist) => DirectedWeightedEdge(a, p2, dist) })
        .groupBy(_.from).mapValues(_.filterNot(_.to == start))
    }))
  }

  private def sumFinished(results: Set[Map[PointAndHistory, Int]]): Int = results.toList.map(_.values.max).sum

  private def parseInput(lines: List[String]): Input = {
    val points = for {
      (r, y) <- lines.zipWithIndex
      (elem, x) <- r.zipWithIndex
    } yield Point2D(x, y) -> elem

    val starts = points.filter(_._2 == '@').map(_._1).toSet
    val path = points.filter(_._2 != '#').map(_._1).toSet
    val walls = points.filter(_._2 == '#').map(_._1).toSet
    val keys = points.filter(p => p._2 >= 'a' && p._2 <= 'z').toMap
    val doors = points.filter(p => p._2 >= 'A' && p._2 <= 'Z').toMap

    Input(starts, path, walls, keys, doors)
  }

  private def modifyInput(tunnels: Input): Input = {
    val start = tunnels.starts.head
    val newStarts = start + Point2D(1, 1) :: start + Point2D(-1, -1) :: start + Point2D(1, -1) :: start + Point2D(-1, 1) :: Nil
    val newWalls = start.neighbours + start
    tunnels.copy(starts = newStarts.toSet, path = tunnels.path diff newWalls, walls = tunnels.walls ++ newWalls)
  }

  val input = parseInput(lines)
  val tunnels = mapTunnels(input)

  val modifiedInput = modifyInput(parseInput(lines))
  val modifiedTunnels = mapTunnels(modifiedInput)

  part1(sumFinished(search(input, tunnels)))
  part2(sumFinished(search(modifiedInput, modifiedTunnels)))
}
