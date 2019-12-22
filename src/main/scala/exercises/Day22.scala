package exercises

import util.DayN

object Day22 extends DayN {
  override val num = 22

  type IndexF = (Long, Long) => Long

  def shuffleDeck(deckSize: Long, commands: List[IndexF]): Map[Long, Long] = {
    val commands: List[IndexF] = parseCommands(lines)
    val factoryOrderDeck = (0L until deckSize).zipWithIndex.toMap.mapValues(_.toLong)
    commands.foldLeft(factoryOrderDeck)((b, f) => b.map { case (k, v) => f(k, b.size) -> v })
  }

  def indexTracker(deckSize: Long, repeats: Long, commands: List[IndexF], index: Long): Long = ???

  private def deal[A](i: Long, size: Long): Long = (size - 1) - i
  private def cut[A](v: Long)(i: Long, size: Long): Long = (i - v) % size
  private def dealWithInc[A](v: Long)(i: Long, size: Long): Long = (i * v) % size

  private def parseCommands(commands: List[String]): List[(Long, Long) => Long] = commands.map(command => {
    val parts = command.split(" ")
    if (parts.head == "cut") Day22.cut[Int](parts(1).toInt) _
    else if (parts(2) == "increment") Day22.dealWithInc[Int](parts(3).toInt) _
    else Day22.deal[Int] _
  })

  val deckSize = 10007
  val commands = parseCommands(lines)
  part1(shuffleDeck(10007, commands).filter(_._2 == 2019L))

  val crazyDeckSize: Long = 119315717514047L
  val crazyRepeats: Long = 101741582076661L


}
