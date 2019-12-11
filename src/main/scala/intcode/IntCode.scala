package intcode

object IntCode {
  def parseInput(s: String): Map[Long, Long] = s.split(",").map(_.toLong).zipWithIndex.map(p => p._2.toLong -> p._1).toMap

  def run(state: State): State = {
    @annotation.tailrec
    def loop(state: State): State = {
      val s = IntCode.step(state)
      s.status match {
        case Running => loop(s)
        case _ => s
      }
    }

    loop(state)
  }

  def step(state: State): State = {
    val code = state.ints(state.index)
    val op = code % 100
    val modes: IndexedSeq[ParameterMode] = code.toString.reverse.drop(2).map {
      case '0' => PositionMode
      case '1' => ImmediateMode
      case _ => RelativeMode(state.relativeBase)
    }

    val command = op match {
      case 1 => Add(state, modes)
      case 2 => Multiply(state, modes)
      case 3 => Input(state, modes)
      case 4 => Output(state, modes)
      case 5 => JumpIfTrue(state, modes)
      case 6 => JumpIfFalse(state, modes)
      case 7 => LessThan(state, modes)
      case 8 => Equals(state, modes)
      case 9 => AdjustRelativeOffset(state, modes)
      case _ => Finish(state)
    }

    command.run()
  }
}
