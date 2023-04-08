package zio

sealed trait Exit[+E, +A]

object Exit {
  final case class Success[A](a: A)        extends Exit[Nothing, A]
  final case class Failure[E](e: Cause[E]) extends Exit[E, Nothing]
}