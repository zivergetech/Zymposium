package zio

sealed trait RuntimeFlag
object RuntimeFlag {
  case object Interruptible extends RuntimeFlag
}
