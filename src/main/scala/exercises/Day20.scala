package exercises

import util._
import util.geometry.direction.Direction2D
import util.geometry.point.{Point2D, Point3D}
import util.search.BFS

object Day20 extends DayN {
  override val num = 20

  case class Portal(label: String, entrance: Point2D, exit: Point2D, tierChange: Int)

  case class Doughnut(path: Set[Point2D], neighbours: Map[Point2D, Set[Point3D]], portals: Set[Portal])

  def shortestPath(doughnut: Doughnut): Int = {
    def next(p: Point3D): Set[Point3D] =
      doughnut.neighbours.getOrElse(p.dropZ, Set()).map(_ + p).filter(_.z >= 0)

    val start = doughnut.portals.find(_.label == "AA").get.exit.addZ(0)
    val target = doughnut.portals.find(_.label == "ZZ").get.exit.addZ(0)
    val distances = BFS.searchWithExit(start)(_ == target)(next)((_, trail) => Some(trail.size))
    distances(target)
  }

  private def parseInput(lines: List[String], tiered: Boolean): Doughnut = {
    def extractPortal(p: Point2D, portalPoint2Ds: Map[Point2D, Char]): Portal = {
      Direction2D.all.flatMap(d => {
        val n = d.mutation + p
        if (portalPoint2Ds(n).isUpper) {
          val name = List(portalPoint2Ds(n), portalPoint2Ds(d.mutation + n)).mkString
          val tierChange = if (!tiered) 0 else if (portalPoint2Ds.contains(d.mutation * 3 + p)) 1 else -1
          if (d.englishLanguageDir) Some(Portal(name, n, p, tierChange))
          else Some(Portal(name.reverse, n, p, tierChange))
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
    val portalNeighbours = portals.flatMap(ps => for {p1 <- ps; p2 <- ps; if p1 != p2} yield p1.entrance -> p2.exit.addZ(p1.tierChange)).toMap
    val neighboursChoices = path.map(p => p -> p.neighbours.map(n => portalNeighbours.getOrElse(n, n.addZ(0)) - p))
    val neighbours = neighboursChoices.map { case (p, ns) => p -> ns.filter(p3 => path.contains(p + p3.dropZ)) }.toMap

    Doughnut(path, neighbours, portals.flatten.toSet)
  }

  val example = "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       ".split("\n").toList
  val doughnut = parseInput(lines, false)
  part1(shortestPath(doughnut))

  val example2 = "             Z L X W       C                 \n             Z P Q B       K                 \n  ###########.#.#.#.#######.###############  \n  #...#.......#.#.......#.#.......#.#.#...#  \n  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \n  #.#...#.#.#...#.#.#...#...#...#.#.......#  \n  #.###.#######.###.###.#.###.###.#.#######  \n  #...#.......#.#...#...#.............#...#  \n  #.#########.#######.#.#######.#######.###  \n  #...#.#    F       R I       Z    #.#.#.#  \n  #.###.#    D       E C       H    #.#.#.#  \n  #.#...#                           #...#.#  \n  #.###.#                           #.###.#  \n  #.#....OA                       WB..#.#..ZH\n  #.###.#                           #.#.#.#  \nCJ......#                           #.....#  \n  #######                           #######  \n  #.#....CK                         #......IC\n  #.###.#                           #.###.#  \n  #.....#                           #...#.#  \n  ###.###                           #.#.#.#  \nXF....#.#                         RF..#.#.#  \n  #####.#                           #######  \n  #......CJ                       NM..#...#  \n  ###.#.#                           #.###.#  \nRE....#.#                           #......RF\n  ###.###        X   X       L      #.#.#.#  \n  #.....#        F   Q       P      #.#.#.#  \n  ###.###########.###.#######.#########.###  \n  #.....#...#.....#.......#...#.....#.#...#  \n  #####.#.###.#######.#######.###.###.#.#.#  \n  #.......#.......#.#.#.#.#...#...#...#.#.#  \n  #####.###.#####.#.#.#.#.###.###.#.###.###  \n  #.......#.....#.#...#...............#...#  \n  #############.#.#.###.###################  \n               A O F   N                     \n               A A D   M                     ".split("\n").toList
  val tieredDoughnut = parseInput(lines, true)
  part2(shortestPath(tieredDoughnut))
}
