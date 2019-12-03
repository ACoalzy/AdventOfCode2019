package util

import scala.io.Source

trait DayN extends App {

  val num: Int

  lazy val lines: List[String] = Source.fromResource(s"day$num.txt").getLines.toList

  def part1[A](result: A) = println(s"Part 1: $result")
  def part2[A](result: A) = println(s"Part 2: $result")

}
