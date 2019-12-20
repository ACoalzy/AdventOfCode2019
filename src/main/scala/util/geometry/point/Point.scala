package util.geometry.point

import util.geometry.direction.Direction

trait Point {
  def +(p: Point): Point
  def -(p: Point): Point
  def *(p: Point): Point
  def *(i: Int): Point
  def rotate(dir: Long)
  def manHattanDist(p: Point)
  def to(target: Point, direction: Direction)
  def neighbours: Set[Point]
}
