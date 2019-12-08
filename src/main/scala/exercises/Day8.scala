package exercises

import util.DayN

object Day8 extends DayN {
  override val num = 8

  def corruptionCheck(pixels: String, width: Int, height: Int) = {
    val layers = pixels.grouped(width * height)
    val minLayer = layers.minBy(l => l.count(_ == '0'))
    minLayer.count(_ == '1') * minLayer.count(_ == '2')
  }

  def decodeMessage(pixels: String, width: Int, height: Int): String = {
    val layers = pixels.grouped(width * height).toList
    val result = 0 until (width * height) map { i =>
      layers.map(_.charAt(i)).find(_ != '2') match {
        case None => " "
        case Some('0') => "."
        case Some('1') => "#"
      }
    }

    result.grouped(width).map(_.mkString).mkString("\n")
  }

  val input = lines.head
  part1(corruptionCheck(input, 25, 6))
  part2("\n" + decodeMessage(input, 25, 6))
}
