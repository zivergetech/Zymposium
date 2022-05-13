package stm

import zio._

sealed trait STM[+A] { self =>

  def commit: UIO[A] =
    ZIO.succeed {
      var loop = true
      var a    = null.asInstanceOf[A]
      while (loop) {
        val journal = Journal.make()
        a = self.run(journal)
        val analysisResult = journal.analyze
        analysisResult match {
          case AnalysisResult.ReadOnly =>
            withGlobalLock {
              if (journal.isValid) {
                loop = false
              }
            }
          case AnalysisResult.ReadWrite =>
            withGlobalLock {
              if (journal.isValid) {
                journal.commit()
                loop = false
              }
            }
          case AnalysisResult.Invalid =>
        }
      }
      a
    }

  def flatMap[B](f: A => STM[B]): STM[B] =
    STM.FlatMap(self, f)

  def map[B](f: A => B): STM[B] =
    flatMap(a => STM.succeed(f(a)))

  private def run(journal: Journal): A =
    self match {
      case STM.Succeed(a) => a()
      case STM.FlatMap(stm, f) =>
        f(stm.run(journal)).run(journal)
      case STM.Transaction(transaction) =>
        transaction(journal)
    }

  private def withGlobalLock[A](a: => A): A =
    STM.synchronized(a)
}

object STM {

  def succeed[A](a: => A): STM[A] =
    Succeed(() => a)

  final case class Succeed[A](a: () => A)                     extends STM[A]
  final case class FlatMap[A, B](stm: STM[A], f: A => STM[B]) extends STM[B]
  final case class Transaction[A](transaction: Journal => A)  extends STM[A]
}
