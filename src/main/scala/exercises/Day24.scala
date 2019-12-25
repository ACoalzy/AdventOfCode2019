package exercises

import util.DayN
import util.geometry.point.{Point2D, Point3D}

object Day24 extends DayN {
  override val num = 24

  val size = 5
  val range = 0 until size

  def repeatedBiodiversity(init: Map[Point2D, Char]): Int = {
    @annotation.tailrec
    def loop(state: Map[Point2D, Char], hist: Set[Map[Point2D, Char]]): Map[Point2D, Char] =
      if (hist.contains(state)) state
      else loop(simulate(state), hist + state)

    loop(init, Set()).filter(_._2 == '#').keySet.map(p => math.pow(2.0, (p.y * size) + p.x)).sum.toInt
  }

  def recursiveSimulation(init: Map[Point2D, Char], minutes: Int): Map[Point3D, Char] = {
    @annotation.tailrec
    def loop(state: Map[Point3D, Char], iter: Int): Map[Point3D, Char] = {
      if (iter == minutes) state
      else loop(simulate3D(if (iter % 2 == 0) wrap(state, (iter / 2) + 1) else state), iter + 1)
    }

    loop(init.map { case (k, v) => k.addZ(0) -> v }, 0)
  }

  private def simulate(map: Map[Point2D, Char]): Map[Point2D, Char] = {
    val bugs = map.filter(_._2 == '#').keySet
    map.map { case (p, c) =>
      val adjacents: Int = bugs.count(p.neighbours)
      if (c == '#' && adjacents != 1) p -> '.'
      else if (c == '.' && Set(1, 2).contains(adjacents)) p -> '#'
      else p -> c
    }
  }

  private def simulate3D(map: Map[Point3D, Char]): Map[Point3D, Char] = {
    def transform(op: Point3D, p: Point3D): Set[Point3D] = {
      if (p.x == -1) Set(Point3D(1, 2, p.z + 1))
      else if (p.y == -1) Set(Point3D(2, 1, p.z + 1))
      else if (p.x == 5) Set(Point3D(3, 2, p.z + 1))
      else if (p.y == 5) Set(Point3D(2, 3, p.z + 1))
      else if (p.x == 2 && p.y == 2 && op.x == 1) range.map(y => Point3D(0, y, p.z - 1)).toSet
      else if (p.x == 2 && p.y == 2 && op.y == 1) range.map(x => Point3D(x, 0, p.z - 1)).toSet
      else if (p.x == 2 && p.y == 2 && op.x == 3) range.map(y => Point3D(4, y, p.z - 1)).toSet
      else if (p.x == 2 && p.y == 2 && op.y == 3) range.map(x => Point3D(x, 4, p.z - 1)).toSet
      else Set(p)
    }
    val bugs = map.filter(_._2 == '#').keySet
    map.map { case (p, c) =>
      val neighbours = p.neighbours2d.flatMap(np => transform(p, np))
      val adjacents = bugs.count(neighbours)
      if (p.x == 2 && p.y == 2) p -> '?'
      else if (c == '#' && adjacents != 1) p -> '.'
      else if (c == '.' && Set(1, 2).contains(adjacents)) p -> '#'
      else p -> c
    }
  }

  private def wrap(map: Map[Point3D, Char], z: Int): Map[Point3D, Char] = {
    val layer = for {x <- range; y <- range} yield Point2D(x, y)
    map ++ layer.map(_.addZ(z) -> '.') ++ layer.map(_.addZ(-z) -> '.')
  }

  private def parseInput(lines: List[String]): Map[Point2D, Char] = (for {
    (l, y) <- lines.zipWithIndex
    (elem, x) <- l.zipWithIndex
  } yield Point2D(x, y) -> elem).toMap

  val example = "....#\n#..#.\n#..##\n..#..\n#....".split("\n").toList
  val input = parseInput(lines)
  part1(repeatedBiodiversity(input))
  part2(recursiveSimulation(input, 200).count(_._2 == '#'))

}
