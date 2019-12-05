package intcode

case class State(index: Int, ints: Vector[Int]) {

  def jumpIf(lpm: ParameterMode, rpm: ParameterMode)(condition: Int => Boolean, default: Int): Int = {
    if (condition(lpm.value(ints)(index + 1))) rpm.value(ints)(index + 2)
    else index + default
  }

  def threeIntOp(lpm: ParameterMode, rpm: ParameterMode, ipm: ParameterMode)(op: (Int, Int) => Int): Vector[Int] =
    ints.updated(ipm.index(ints)(index + 3), op(lpm.value(ints)(index + 1), rpm.value(ints)(index + 2)))

  def store(v: Int)(pm: ParameterMode): Vector[Int] = ints.updated(pm.index(ints)(index + 1), v)

}
