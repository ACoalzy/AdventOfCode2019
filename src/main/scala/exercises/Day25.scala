package exercises

import intcode.{IntCode, State}
import util.DayN

import scala.collection.immutable.Queue
import scala.io.StdIn

object Day25 extends DayN {
  override val num = 25

  // commands:
  // north, south, east, west
  // take <name of item>
  // drop <name of item>
  // inv

  // bad items: molten lava, giant electromagnet, infinite loop, escape pod, photons

  def runGame(state: State): Unit = {
    @annotation.tailrec
    def loop(state: State): Unit = {
      val input = StdIn.readLine()
      val newState = if (input.startsWith("bf")) bruteForceDoor(state, input.drop(3)) else state.copy(output = Queue(), input = Queue() ++ (input + "\n").map(_.toLong))
      loop(IntCode.asciiDebugRun(newState))
    }

    loop(IntCode.asciiDebugRun(state))
  }

  private def bruteForceDoor(state: State, command: String): State = {
    val inventory = IntCode.run(state.copy(output = Queue(), input = Queue() ++ "inv\n".map(_.toLong)))
      .output.toList.map(_.toChar).mkString
      .split("\n").toList.drop(2).dropRight(2).map(_.drop(2))
    val droppedState = IntCode.run(state.copy(output = Queue(), input = Queue() ++ inventory.map(item => s"drop $item\n").mkString.map(_.toLong)))
    val combinations = inventory.toSet.subsets.map(_.toList).toList
    val attempts = combinations.map(combo => {
      val takenState = IntCode.run(droppedState.copy(output = Queue(), input = Queue() ++ combo.map(item => s"take $item\n").mkString.map(_.toLong)))
      IntCode.run(takenState.copy(output = Queue(), input = Queue() ++ (command + "\n").map(_.toLong)))
    })

    attempts.filterNot(s => s.output.toList.map(_.toChar).mkString.contains("Security Checkpoint")).head
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(runGame(State(0, input)))
}
