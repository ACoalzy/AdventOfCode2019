package intcode

object IntCode {
  def step(state: State): State = {
    val code = state.ints(state.index)
    val op = code % 100
    val modes: IndexedSeq[ParameterMode] = code.toString.reverse.drop(2).map {
      case '0' => PositionMode
      case _ => ImmediateMode
    }

    val command = op match {
      case 99 => Finish(state)
      case 1 => Add(state, modes)
      case 2 => Multiply(state, modes)
      case 3 => Input(state, modes)
      case 4 => Output(state, modes)
      case 5 => JumpIfTrue(state, modes)
      case 6 => JumpIfFalse(state, modes)
      case 7 => LessThan(state, modes)
      case 8 => Equals(state, modes)
    }

    command.run()
  }
}
