package util

import util.geometry.point.Point2D

object Display {

  def displayMap[A](map: Map[Point2D, A])(f: A => Char): String = {
    val minX = map.keySet.map(_.x).min
    val maxX = map.keySet.map(_.x).max
    val minY = map.keySet.map(_.y).min
    val maxY = map.keySet.map(_.y).max

    val matrix = Array.ofDim[Char](maxY - minY + 1, maxX - minX + 1)
    map.foreach { case (p, l) => matrix(p.y - minY)(p.x - minX) = f(l) }

    matrix.map(_.mkString("")).reverse.mkString("\n")
  }

}
