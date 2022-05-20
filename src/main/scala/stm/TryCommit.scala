package stm

sealed trait TryCommit[+A]

object TryCommit {
  final case class Done[+A](a: A) extends TryCommit[A]
  final case class Suspend(journal: Journal) extends TryCommit[Nothing]
}