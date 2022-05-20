package stm

import zio._

sealed trait STM[+A] { self =>

  def commit: UIO[A] =
    STM.atomically(self)

  def flatMap[B](f: A => STM[B]): STM[B] =
    STM.FlatMap(self, f)

  def map[B](f: A => B): STM[B] =
    flatMap(a => STM.succeed(f(a)))

  private def run(journal: Journal): TExit[A] =
    self match {
      case STM.Succeed(a) => TExit.Done(a())
      case STM.FlatMap(stm, f) =>
        stm.run(journal) match {
          case TExit.Done(a) => f(a).run(journal)
          case TExit.Retry   => TExit.Retry
        }
      case STM.Transaction(transaction) =>
        TExit.Done(transaction(journal))
      case STM.Retry =>
        TExit.Retry
    }
}

object STM {

  def atomically[A](stm: STM[A]): UIO[A] =
    ZIO.executor.flatMap { executor =>
      ZIO.suspendSucceed {
        tryCommitSync(stm, executor) match {
          case TryCommit.Done(a) => ZIO.succeed(a)
          case TryCommit.Suspend(journal) =>
            val transactionId = TransactionId.make()
            ZIO.async(tryCommitAsync(stm, transactionId, executor, journal))
        }
      }
    }

  val retry: STM[Nothing] =
    STM.Retry

  def succeed[A](a: => A): STM[A] =
    Succeed(() => a)


  private def completeToDos(journal: Journal, executor: Executor): Unit = {
    val todos = journal.collectToDos()
    executeToDos(todos, executor)
  }

  private def executeToDos(todos: Map[TransactionId, ToDo], executor: Executor): Unit =
    executor.unsafeSubmitOrThrow(() => todos.foreach { case (_, todo) => todo() })

  private def tryCommitSync[A](stm: STM[A], executor: Executor): TryCommit[A] = {
    var loop = true
    var exit    = null.asInstanceOf[TExit[A]]
    var journal = null.asInstanceOf[Journal]
    while (loop) {
      journal = Journal.make()
      exit = stm.run(journal)
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
    exit match {
      case TExit.Done(a) =>
        completeToDos(journal, executor)
        TryCommit.Done(a)
      case TExit.Retry   =>
        TryCommit.Suspend(journal)
    }
  }

  def tryCommitAsync[R, E, A](stm: STM[A], transactionId: TransactionId, executor: Executor, journal: Journal)(cb: ZIO[R, E, A] => Unit): Unit = {

    def complete(a: A): Unit =
      cb(ZIO.succeed(a))

    def suspend(journal: Journal): Unit = {
      journal.addToDo(transactionId, () => tryCommitAsync(stm, transactionId, executor, null)(cb))
    }

    if (journal eq null) {
      tryCommitSync(stm, executor) match {
        case TryCommit.Done(a) => complete(a)
        case TryCommit.Suspend(journal) => suspend(journal)
      }
    } else suspend(journal)
  }

  private def withGlobalLock[A](a: => A): A =
    STM.synchronized(a)

  final case class Succeed[A](a: () => A)                     extends STM[A]
  final case class FlatMap[A, B](stm: STM[A], f: A => STM[B]) extends STM[B]
  final case class Transaction[A](transaction: Journal => A)  extends STM[A]
  case object Retry                                           extends STM[Nothing]
}
