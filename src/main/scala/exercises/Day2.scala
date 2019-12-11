package exercises

import intcode.{IntCode, State}
import util.DayN

import scala.collection.immutable.Queue

object Day2 extends DayN {
  override val num = 2

  def nounVerbFinder(lowerLimit: Long, upperLimit: Long, target: Long, ints: Map[Long, Long]): Option[(Long, Long)] = {
    val range = lowerLimit.to(upperLimit)
    val results = for {
      noun <- range
      verb <- range
    } yield (noun, verb) -> IntCode.run(State(0, setupNounAndVerb(noun, verb, ints), Queue()))

    results.find(_._2.ints(0) == target).map(_._1)
  }

  private def setupNounAndVerb(noun: Long, verb: Long, ints: Map[Long, Long]): Map[Long, Long] =
    ints.updated(1, noun).updated(2, verb)

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(IntCode.run(State(0, setupNounAndVerb(12, 2, input), Queue())).ints(0))

  val result = nounVerbFinder(0, 99, 19690720, input)
  part2(result.map { case(noun, verb) => 100 * noun + verb })
}
