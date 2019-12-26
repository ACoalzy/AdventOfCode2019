package util.search

import scala.collection.mutable

object BFS {

  def distance[A](a: A, trail: Vector[A]): Option[Int] = Some(trail.size)

  def fullSearch[A, B](start: A)(neighbours: A => Set[A])(f: (A, Vector[A]) => Option[B]) =
    doSearch(start)(_ => false)(neighbours)(f)

  def searchWithExit[A, B](start: A)(exit: A => Boolean)(neighbours: A => Set[A])(f: (A, Vector[A]) => Option[B]) =
    doSearch(start)(exit)(neighbours)(f)

  private def doSearch[A, B](start: A)(exit: A => Boolean)(neighbours: A => Set[A])(f: (A, Vector[A]) => Option[B]) = {
    case class LoopState(a: A, trail: Vector[A] = Vector())

    val queue: mutable.Queue[LoopState] = mutable.Queue(LoopState(start))
    val history: mutable.Set[A] = mutable.Set.empty[A]
    val acc: mutable.Map[A, B] = mutable.Map.empty[A, B]

    @annotation.tailrec
    def loop(): Map[A, B] = {
      queue.dequeueFirst(_ => true) match {
        case None => acc.toMap
        case Some(state) if history.contains(state.a) => loop()
        case Some(state) =>
          val ns = neighbours(state.a).diff(history).toList
          ns.foreach(n => queue.enqueue(LoopState(n, state.trail :+ state.a)))
          history += state.a
          f(state.a, state.trail).foreach(b => acc += (state.a -> b))
          if (exit(state.a)) acc.toMap
          else loop()
      }
    }

    loop()
  }

}
