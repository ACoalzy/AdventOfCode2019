package exercises

import util.DayN

object Day4 extends DayN {
  override val num = 4

  sealed trait Filter[A] {
    def apply(a: A): Boolean
  }

  case object AlwaysIncreasing extends Filter[String] {
    override def apply(s: String): Boolean =
      if (s.length > 1) 1 until s.length forall (i => s.charAt(i) >= s.charAt(i - 1))
      else true
  }

  case object ContainsAdjacentPair extends Filter[String] {
    override def apply(s: String): Boolean =
      if (s.length > 1) 1 until s.length exists (i => s.charAt(i) == s.charAt(i - 1))
      else false
  }

  case object ContainsExclusivePair extends Filter[String] {
    override def apply(s: String): Boolean = {
      val chars = s.toSet
      chars.map(c => s.count(_ == c)).contains(2)
    }
  }

  def validate[A](a: A)(rules: List[Filter[A]]): Boolean = rules.forall(_ (a))

  val range = 245182 to 790572
  part1(range.count(i => validate(i.toString)(List(AlwaysIncreasing, ContainsAdjacentPair))))
  part2(range.count(i => validate(i.toString)(List(AlwaysIncreasing, ContainsExclusivePair))))
}
