package exercises

import util._
import util.geometry.direction.Direction2D
import util.geometry.edge.DirectedWeightedEdge
import util.geometry.point.{Point2D, Point3D}
import util.search.{BFS, Dijkstra}

object Day20 extends DayN {
  override val num = 20

  case class Portal(label: String, entrance: Point2D, exit: Point3D)

  case class Doughnut(path: Set[Point2D], neighbours: Map[Point2D, Set[Point2D]], jumps: Map[Point2D, Set[Point3D]], portals: Set[Portal])

  case class DoughnutSearch(start: Point3D, target: Point3D, edges: Set[DirectedWeightedEdge[Point3D]])

  def shortestPath(search: DoughnutSearch): Option[Int] = {
    val edgeMap = search.edges.groupBy(_.from)

    def next(p: Point3D): Set[DirectedWeightedEdge[Point3D]] =
      edgeMap.getOrElse(p.copy(z = 0), Set())
        .map(e => e.copy(to = e.to.copy(z = e.to.z + p.z)))
        .filter(_.to.z >= 0)

    Dijkstra.searchWithExit(search.start)(_ == search.target)(next).get(search.target)
  }

  private def defineSearch(doughnut: Doughnut): DoughnutSearch = {
    def next(p: Point2D): Set[Point2D] = doughnut.neighbours.getOrElse(p, Set())

    def distance(a: Point2D, trail: Vector[Point2D]): Option[Int] =
      if (doughnut.portals.map(_.exit.dropZ).contains(a)) Some(trail.size).filter(_ > 0) else None

    val edges = doughnut.portals.flatMap(portal => {
      BFS.fullSearch(portal.exit.dropZ)(next)(distance)
        .map { case (p2, weight) => DirectedWeightedEdge(portal.exit.copy(z = 0), p2.addZ(0), weight) }
    })

    val jumps = doughnut.portals.flatMap(portal => {
      doughnut.jumps(portal.exit.dropZ).headOption.map(j => DirectedWeightedEdge[Point3D](portal.exit.copy(z = 0), j + portal.exit.dropZ, 1))
    })

    val start = doughnut.portals.find(_.label == "AA").get.exit.copy(z = 0)
    val finish = doughnut.portals.find(_.label == "ZZ").get.exit.copy(z = 0)
    DoughnutSearch(start, finish, edges ++ jumps)
  }

  private def parseInput(lines: List[String], tiered: Boolean): Doughnut = {
    def extractPortal(p: Point2D, portalPoint2Ds: Map[Point2D, Char]): Portal = {
      Direction2D.all.flatMap(d => {
        val n = d.mutation + p
        if (portalPoint2Ds(n).isUpper) {
          val name = List(portalPoint2Ds(n), portalPoint2Ds(d.mutation + n)).mkString
          val tierChange = if (!tiered) 0 else if (portalPoint2Ds.contains(d.mutation * 3 + p)) -1 else 1
          if (d.englishLanguageDir) Some(Portal(name, n, p.addZ(tierChange)))
          else Some(Portal(name.reverse, n, p.addZ(tierChange)))
        } else None
      }).head
    }

    val pointMap = (for {
      (row, y) <- lines.zipWithIndex
      (elem, x) <- row.zipWithIndex
    } yield Point2D(x, y) -> elem).toMap

    val path = pointMap.filter(_._2 == '.').keySet
    val portalPoints = pointMap.filter(_._2.isUpper)

    val portals = path.filter(p => p.neighbours.exists(portalPoints.contains)).map(p => extractPortal(p, pointMap)).groupBy(_.label).values
    val jumpLocations = portals.flatMap(ps => for {p1 <- ps; p2 <- ps; if p1 != p2} yield p1.entrance -> p2.exit).toMap
    val jumps = path.map(p => p -> p.neighbours.flatMap(n => jumpLocations.get(n).map(_ - p))).toMap
    val neighbours = path.map(p => p -> p.neighbours.intersect(path)).toMap

    Doughnut(path, neighbours, jumps, portals.flatten.toSet)
  }

  val doughnut = parseInput(lines, false)
  part1(shortestPath(defineSearch(doughnut)))

  val tieredDoughnut = parseInput(lines, true)
  part2(shortestPath(defineSearch(tieredDoughnut)))
}
