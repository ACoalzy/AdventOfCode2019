package exercises

import intcode.{Add, Multiply, State}
import util.DayN

object Day2 extends DayN {
  override val num = 2

  def intCodeRunner(noun: Int, verb: Int, ints: Vector[Int]): Int = {
    @annotation.tailrec
    def loop(state: State): Vector[Int] = {
      val next = state.ints(state.index) match {
        case 99 => None
        case 1 => Some(Add(state))
        case 2 => Some(Multiply(state))
      }

      next.map(_.run) match {
        case None => state.ints
        case Some(s) => loop(s)
      }
    }

    loop(State(0, setupNounAndVerb(noun, verb, ints), List())).head
  }

  def nounVerbFinder(lowerLimit: Int, upperLimit: Int, target: Int, ints: Vector[Int]): Option[(Int, Int)] = {
    val range = lowerLimit.to(upperLimit)
    val results = for {
      noun <- range
      verb <- range
    } yield (noun, verb) -> intCodeRunner(noun, verb, ints)

    results.find(_._2 == target).map(_._1)
  }

  private def setupNounAndVerb(noun: Int, verb: Int, ints: Vector[Int]): Vector[Int] =
    ints.updated(1, noun).updated(2, verb)

  val input: Vector[Int] = lines.head.split(",").map(_.toInt).toVector

  part1(intCodeRunner(12, 2, input))

  val result = nounVerbFinder(0, 99, 19690720, input)
  part2(result.map { case(noun, verb) => 100 * noun + verb })
}
