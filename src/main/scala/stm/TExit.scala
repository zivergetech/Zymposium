package stm

sealed trait TExit[+A]

object TExit {
  final case class Done[+A](a: A) extends TExit[A]
  case object Retry extends TExit[Nothing]
}