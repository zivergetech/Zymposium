package stm

sealed trait State { self =>
  def isRunning: Boolean =
    self match {
      case State.Running => true
      case State.Done    => false
    }
}

object State {
  case object Running extends State
  case object Done extends State
}