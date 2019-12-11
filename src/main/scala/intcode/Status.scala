package intcode

sealed trait Status

case object Running extends Status

case object Waiting extends Status

case object Finished extends Status
