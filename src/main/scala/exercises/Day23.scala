package exercises

import intcode.{IntCode, State}
import util.DayN

import scala.collection.immutable.Queue

object Day23 extends DayN {
  override val num = 23

  case class OutputInstr(addr: Long, x: Long, y: Long) {
    val asVector: Vector[Long] = Vector(x, y)
  }

  def runMachines(state: State, count: Long, target: Long): Long = {
    @annotation.tailrec
    def loop(machines: Map[Long, State]): Long = {
      val newMachines = machines.mapValues(IntCode.run)
      val outputs = newMachines.values.map(_.output).flatMap(parseOutput).toVector.groupBy(_.addr)
      val targetOut = outputs.get(target)
      if (targetOut.isEmpty) {
        val updatedMachines = newMachines.map { case (i, s) => i ->
          s.copy(output = Queue(), input = outputs.get(i).map(os => Queue() ++ os.flatMap(_.asVector)).getOrElse(Queue(-1)))
        }
        loop(updatedMachines)
      } else targetOut.get.head.y
    }
    val machines = (0L until count).map(i => i -> state.copy(input = Queue(i))).toMap

    loop(machines)
  }

  def runNatMachines(state: State, count: Long, natAddr: Long): Long = {
    @annotation.tailrec
    def loop(machines: Map[Long, State], nat: Option[OutputInstr], natYHistory: Set[Long]): Long = {
      val newMachines = machines.mapValues(IntCode.run)
      val outputs = newMachines.values.map(_.output).flatMap(parseOutput).toVector.groupBy(_.addr)
      val nats = outputs.get(natAddr)
      val updatedNat = if (nats.nonEmpty) nats.map(_.last) else nat
      if (outputs.nonEmpty || updatedNat.isEmpty) {
        val updatedMachines = newMachines.map { case (i, s) => i ->
          s.copy(output = Queue(), input = outputs.get(i).map(os => Queue() ++ os.flatMap(_.asVector)).getOrElse(Queue(-1)))
        }
        loop(updatedMachines, updatedNat, natYHistory)
      } else if (natYHistory.contains(updatedNat.get.y))
        updatedNat.get.y
      else {
        loop(newMachines.updated(0, newMachines(0).copy(input = Queue() ++ updatedNat.get.asVector)).mapValues(_.copy(output = Queue())), None, natYHistory + updatedNat.get.y)
      }
    }
    val machines = (0L until count).map(i => i -> state.copy(input = Queue(i))).toMap

    loop(machines, None, Set())
  }

  private def parseOutput(output: Queue[Long]): List[OutputInstr] = {
    def parse(o: Queue[Long]) = for {
      (addr, r) <- o.dequeueOption
      (x, r2) <- r.dequeueOption
      (y, _) <- r2.dequeueOption
    } yield OutputInstr(addr, x, y)

    output.grouped(3).flatMap(parse).toList
  }

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)

  part1(runMachines(State(0, input), 50L, 255L))
  part2(runNatMachines(State(0, input), 50L, 255L))
}
