package zio

sealed trait Cause[+E]

// ZIO[Nothing, Int] = ZIO.succeed(2 + 2 / 0))

object Cause {

  final case class Fail[E](error: E)         extends Cause[E]
  final case class Die(throwable: Throwable) extends Cause[Nothing]
  case object Interrupt                      extends Cause[Nothing]

  def interrupt: Cause[Nothing] =
    Interrupt

  def fail[E](e: E): Cause[E] =
    Fail(e)

  def die(throwable: Throwable): Cause[Nothing] = Die(throwable)
}