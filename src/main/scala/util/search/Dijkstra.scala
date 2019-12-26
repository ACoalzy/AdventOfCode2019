package util.search

import util.geometry.edge.DirectedWeightedEdge

import scala.collection.mutable

object Dijkstra {

  def fullSearch[A, B](start: A)(neighbours: A => Set[DirectedWeightedEdge[A]]): Map[A, Int] =
    doSearch(start)(_ => false)(neighbours)

  def searchWithExit[A, B](start: A)(exit: A => Boolean)(neighbours: A => Set[DirectedWeightedEdge[A]]): Map[A, Int] =
    doSearch(start)(exit)(neighbours)

  private def doSearch[A, B](start: A)(exit: A => Boolean)(edges: A => Set[DirectedWeightedEdge[A]]): Map[A, Int] = {
    case class LoopState(a: A, trail: Vector[A] = Vector(), dist: Int = 0)

    implicit val reverseOrder: Ordering[LoopState] = (a, b) => b.dist compare a.dist
    val queue: mutable.PriorityQueue[LoopState] = mutable.PriorityQueue(LoopState(start))
    val acc: mutable.Map[A, Int] = mutable.Map.empty[A, Int]

    @annotation.tailrec
    def loop(): Map[A, Int] = {
      if (queue.isEmpty) acc.toMap
      else {
        val state = queue.dequeue()
        if (acc.get(state.a).exists(_ <= state.dist)) loop()
        else {
          val es = edges(state.a).toList
          es.foreach(n => queue.enqueue(LoopState(n.to, state.trail :+ state.a, state.dist + n.weight)))
          acc += state.a -> state.dist
          if (exit(state.a)) acc.toMap
          else loop()
        }
      }
    }

    loop()
  }

}
