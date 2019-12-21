package exercises

import intcode.{IntCode, State}
import util.DayN

import scala.collection.immutable.Queue

object Day21 extends DayN{
  override val num = 21

  def debugSpringScript(state: State): Unit = println(IntCode.run(state).output.map(_.toChar).mkString)

  def runSpringScript(state: State): Long = IntCode.run(state).output.last

  private def initState(commands: List[String], intcode: Map[Long, Long]): State =
    State(0, intcode, Queue() ++ commands.map(_ + "\n").mkString.map(_.toLong))

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  val walk = List(
    "NOT J J",
    "AND A J",
    "AND B J",
    "AND C J",
    "OR D T",
    "NOT J J",
    "AND T J",
    "WALK"
  )

  val state = initState(walk, input)
  part1(runSpringScript(state))

  val run = List(
    "OR D T",
    "OR H J",
    "OR E J",
    "AND J T",
    "AND A J",
    "AND B J",
    "AND C J",
    "NOT J J",
    "AND T J",
    "RUN"
  )
  val runState = initState(run, input)
  part2(runSpringScript(runState))
}
