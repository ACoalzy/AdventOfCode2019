package exercises

import util.DayN

object Day4 extends DayN {
  override val num = 4

  sealed trait Filter[A] {
    def apply(a: A): Boolean
  }

  case object AlwaysIncreasing extends Filter[String] {
    override def apply(s: String): Boolean = s.zip(s.drop(1)) forall (cs => cs._1 <= cs._2)
  }

  case object ContainsAdjacentPair extends Filter[String] {
    override def apply(s: String): Boolean = s.zip(s.drop(1)) exists (cs => cs._1 == cs._2)
  }

  case object ContainsExclusivePair extends Filter[String] {
    override def apply(s: String): Boolean = s.groupBy(identity).values.exists(_.length == 2)
  }

  def validate[A](a: A)(rules: List[Filter[A]]): Boolean = rules.forall(_ (a))

  val range = 245182 to 790572
  part1(range.count(i => validate(i.toString)(List(AlwaysIncreasing, ContainsAdjacentPair))))
  part2(range.count(i => validate(i.toString)(List(AlwaysIncreasing, ContainsExclusivePair))))
}
