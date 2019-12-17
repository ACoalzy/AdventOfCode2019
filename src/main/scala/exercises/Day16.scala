package exercises

import util.{DayN, Timer}

object Day16 extends DayN {
  override val num = 16

  def fft(input: String, iterations: Int): List[Int] = {
    (1 to iterations).foldLeft(input.map(_.toInt - 48).toList)((l, _) => fftPhase(l))
  }

  private def fftPhase(input: List[Int], offset: Int = 0): List[Int] = {
    @annotation.tailrec
    def loop(i: Int, input: List[Int], output: List[Int]): List[Int] = i match {
      case index if index > input.size => output.reverse
      case index =>
        val groups = input.drop(index-1).grouped(offset + index).zipWithIndex.toList
        val additions = groups.collect { case (l, i) if (i % 4) == 0 => l }.flatten.sum
        val subtractions = groups.collect { case (l, i) if (i % 4) == 2 => l }.flatten.sum
        loop(index + 1, input, math.abs((additions - subtractions) % 10) :: output)
    }
    loop(1, input, Nil)
  }

  def assumeFarEnoughFft(input: String, repeats: Int, iterations: Int): List[Int] = {
    val offset = input.take(7).toInt
    val offsettified = (input * repeats).toList.drop(offset)
    (1 to iterations).foldLeft(offsettified.map(_.toInt - 48))((l, _) => additionOnlyFftPhase(l.toArray))
  }

  private def additionOnlyFftPhase(input: Array[Int]): List[Int] = {
    @annotation.tailrec
    def loop(i: Int, remaining: Int, output: List[Int]): List[Int] = i match {
      case index if index >= input.length => output.reverse
      case index =>
        loop(index + 1, remaining - input(i), math.abs(remaining % 10) :: output)
    }
    loop(0, input.sum, Nil)
  }

  part1(fft(lines.head, 100).take(8).mkString)
  part2(assumeFarEnoughFft(lines.head, 10000, 100).take(8).mkString)
}
