package util

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.math.Ordering
import scala.math.Ordering.Implicits._

/**
  * An immutable list that maintains a sorted order. At any time, the entire
  * list can be traversed, in sorted order in O(n). A single insert is O(log n),
  * amortized over n inserts.
  */
sealed trait SortedList[A] {
  import SortedList._

  implicit def ord: Ordering[A]

  // Insertion is ammortized O(log n). A merge at level k requires O(2^k) time,
  // but only happens every 2^k elements that get inserted, since there are
  // 2^k-1 vacant spots above this level. That means, if we insert n items,
  // then level k has been merged floor(n / 2^k) times, and thus required
  // floor(n / 2^k) * O(2^k) = O(n) time for all its merges.
  // Now, an n-element sorted list only requires ceil(log n) levels, and so if
  // we sum up the time required for each level individually, we have a time
  // complexity of ceil(log n) * O(n) < 2 * log n * O(n) = O(n log n) to insert
  // n elements in the sorted list.

  def head: A = this match {
    case Level0(a, _) => a
  }

  def tail: SortedList[A] = this match {
    case Level0(_, t) => t
  }

  def +(a: A): SortedList[A] = this match {
    case Level0(b, next) =>
      next.insert(1, if (a < b) Vector(a, b) else Vector(b, a))
    case _ => Level0(a, this)
  }

  def ++(that: List[A]): SortedList[A] = SortedList.merge(this, SortedList(that: _*))

  def ++(that: SortedList[A]): SortedList[A] = SortedList.merge(this, that)

  def size: Int = {
    @tailrec def loop(lvl: SortedList[A], sz: Int): Int = lvl match {
      case Level(_, xs, next) => loop(next, sz + xs.size)
      case Level0(_, next) => loop(next, sz + 1)
      case _ => sz
    }

    loop(this, 0)
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B = iterator.foldLeft(z)(f)

  def map[B: Ordering](f: A => B): SortedList[B] =
    foldLeft(SortedList.empty[B]) { (acc, a) =>
      acc + f(a)
    }

  def flatMap[B: Ordering](f: A => SortedList[B]): SortedList[B] =
    foldLeft(SortedList.empty[B]) { (acc, a) =>
      acc ++ f(a)
    }

  def filter(f: A => Boolean): SortedList[A] =
    foldLeft(SortedList.empty[A]) { (acc, a) =>
      if (f(a)) acc + a else acc
    }

  def iterator: Iterator[A] = {

    // Runtime analysis:
    // For each level, we interleave the current elements with the *previous*
    // iterator, whose size is no greater than half the current level's size.
    // So, the total number of times next is called is: T(k) <= k + T(k / 2),
    // where k is the number of elements in the current level. So, by the time
    // we reach the floor, we have T(n) <= n + T(n / 2) < 2 * T(n) = O(n).

    @tailrec def loop(cur: SortedList[A], it: Iterator[A]): Iterator[A] = cur match {
      case Level(_, elems, next) => loop(next, new Interleaver(elems.iterator, it))
      case Level0(e, next) => loop(next, new Interleaver(Iterator(e), it))
      case _ => it // Floor
    }

    loop(this, Iterator.empty)
  }

  private def insert(rank0: Int, elems0: Vector[A]): SortedList[A] = this match {
    case lvl @ Level(rank, elems, next) =>
      if (rank0 < rank) Level(rank0, elems0, this)
      else if (rank0 > rank) Level(rank, elems, next.insert(rank0, elems0))
      else next.insert(rank + 1, mergeImpl(elems, elems0))

    case Level0(e, next) =>
      Level0(e, next.insert(rank0, elems0))

    case _ => // Floor
      Level(rank0, elems0, this)
  }

  def to[CC[_]](implicit cbf: CanBuildFrom[Nothing, A, CC[A]]): CC[A] =
    iterator.to[CC]

  override def toString: String = iterator.mkString("SortedList(", ", ", ")")
  override def hashCode: Int = 57 * this.iterator.toList.hashCode
  override def equals(that: Any): Boolean = that match {
    case (that: SortedList[_]) => this.iterator sameElements that.iterator
    case _ => false
  }
}

private final case class Level0[A](a: A, next: SortedList[A])(implicit val ord: Ordering[A]) extends SortedList[A]

private final case class Level[A](rank: Int, elems: Vector[A], next: SortedList[A])(implicit val ord: Ordering[A]) extends SortedList[A]

private final class Floor[A](implicit val ord: Ordering[A]) extends SortedList[A]

object SortedList {

  def apply[A: Ordering](xs: A*): SortedList[A] =
    xs.foldLeft(empty[A])(_ + _)

  def empty[A: Ordering]: SortedList[A] = new Floor[A]

  private def mergeImpl[A: Ordering](xs: Vector[A], ys: Vector[A]): Vector[A] =
    new Interleaver(xs.iterator, ys.iterator).toVector

  private def merge[A: Ordering](l0: SortedList[A], l1: SortedList[A]): SortedList[A] = (l0, l1) match {
    case (_: Floor[_], _) => l1
    case (_, _: Floor[_]) => l0
    case (Level0(e0, n0), Level0(e1, n1)) =>
      merge(n0, n1).insert(1, if (e0 <= e1) Vector(e0, e1) else Vector(e1, e0))
    case (Level0(e, n), _) =>
      Level0(e, merge(n, l1))
    case (_, Level0(e, n)) =>
      Level0(e, merge(l0, n))
    case (Level(r0, e0, p0), Level(r1, _, _)) if r0 < r1 =>
      Level(r0, e0, merge(p0, l1))
    case (Level(r0, _, _), Level(r1, e1, p1)) if r1 < r0 =>
      Level(r1, e1, merge(l0, p1))
    case (Level(r, e0, p0), Level(_, e1, p1)) =>
      merge(p0, p1).insert(r + 1, mergeImpl(e0, e1))
  }
}

private final class Interleaver[A: Ordering](lhs: Iterator[A], rhs: Iterator[A]) extends Iterator[A] {
  var lHasNext: Boolean = _
  var lNext: A = _
  var rHasNext: Boolean = _
  var rNext: A = _

  rotateLhs()
  rotateRhs()

  private final def rotateLhs(): A = {
    val old = lNext
    lHasNext = lhs.hasNext
    lNext = if (lHasNext) lhs.next() else null.asInstanceOf[A]
    old
  }

  private final def rotateRhs(): A = {
    val old = rNext
    rHasNext = rhs.hasNext
    rNext = if (rHasNext) rhs.next() else null.asInstanceOf[A]
    old
  }

  def hasNext: Boolean = lHasNext || rHasNext
  def next(): A = {
    if (!lHasNext && !rHasNext) throw new IllegalStateException("next() called on empty iterator")
    else if (!rHasNext) rotateLhs()
    else if (!lHasNext) rotateRhs()
    else if (lNext <= rNext) rotateLhs()
    else rotateRhs()
  }
}