package intcode

sealed trait ParameterMode {
  def index(nums: Map[Long, Long])(i: Long): Long
  def value(nums: Map[Long, Long])(i: Long): Long = nums.getOrElse(index(nums)(i), 0L)
}

case object ImmediateMode extends ParameterMode {
  override def index(nums: Map[Long, Long])(i: Long): Long = i
}

case object PositionMode extends ParameterMode {
  override def index(nums: Map[Long, Long])(i: Long): Long = nums.getOrElse(i, 0L)
}

case class RelativeMode(base: Long) extends ParameterMode {
  override def index(nums: Map[Long, Long])(i: Long): Long = nums.getOrElse(i, 0L) + base
  override def value(nums: Map[Long, Long])(i: Long): Long = nums.getOrElse(index(nums)(i), 0L)
}