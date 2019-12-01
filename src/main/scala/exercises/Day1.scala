package exercises

import util.DayN

object Day1 extends DayN {
  override val num = 1

  def fuelFromMass(modules: List[Int]): List[Int] =
    modules.map(m => (m / 3) - 2)

  def fuelCalculator(modules: List[Int]): Long = {
    @annotation.tailrec
    def loop(masses: List[Int], fuel: Long): Long = fuelFromMass(masses).filter(_ > 0) match {
      case Nil => fuel
      case moreFuel => loop(moreFuel, fuel + moreFuel.sum)
    }

    loop(modules, 0)
  }

  val modules = lines.map(_.toInt)
  part1(fuelFromMass(modules).sum)
  part2(fuelCalculator(modules))
}
