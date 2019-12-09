package exercises

import intcode.{IntCode, State}
import util.DayN

import scala.collection.immutable.Queue

object Day2 extends DayN {
  override val num = 2

  def intCodeRunner(noun: Long, verb: Long, ints: Map[Long, Long]): Long = {
    @annotation.tailrec
    def loop(state: State): Map[Long, Long] = {
      IntCode.step(state) match {
        case s if s.finished => state.ints
        case s => loop(s)
      }
    }

    loop(State(0, setupNounAndVerb(noun, verb, ints), Queue()))(0)
  }

  def nounVerbFinder(lowerLimit: Long, upperLimit: Long, target: Long, ints: Map[Long, Long]): Option[(Long, Long)] = {
    val range = lowerLimit.to(upperLimit)
    val results = for {
      noun <- range
      verb <- range
    } yield (noun, verb) -> intCodeRunner(noun, verb, ints)

    results.find(_._2 == target).map(_._1)
  }

  private def setupNounAndVerb(noun: Long, verb: Long, ints: Map[Long, Long]): Map[Long, Long] =
    ints.updated(1, noun).updated(2, verb)

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(intCodeRunner(12, 2, input))

  val result = nounVerbFinder(0, 99, 19690720, input)
  part2(result.map { case(noun, verb) => 100 * noun + verb })
}
