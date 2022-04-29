import zio._
import zio.stm._
import Transactions.LockStatus.Locked

object Transactions extends ZIOAppDefault {
  // Compose atomic updates

  // Atomic updates are implemented by checking if anyone else has made an
  // update while we were working. If so, we retry, otherwise we finish.

  // Idea of STM
  //
  // 1. "Transaction" - description of our intention to change one or more
  // variables. "I read 0 from variable X and I would like to set it to 1"
  //
  // 2. "Journal" - keeps track of all the variables that we read from when
  // we were creating the transaction. "I read from variable X while
  // constructing this transaction"
  //
  // 3. "Commit" - turns the transaction which described the intention to change
  // the variable into actually changing the variable. "Actually set the
  // variable to 1"

  // 4. Whenever we try to commit a transaction we are going to check the journal
  // and look at every transactional variable that we read from. If any of them
  // were changed by someone else in the meantime then we're going to retry the
  // transaction. If not, we're going to commit the transaction.

  // Ordering 1:

  // Fiber1 - get(0)
  // Fiber1 - set(1)
  // Fiber2 - get(1)
  // Fiber2 - set(2)

  // Ordering 2:

  // Fiber1 - get(0)
  // Fiber2 - get(0)
  // Fiber1 - set(1)
  // Fiber2 - set(1) XXX - retry
  // Fiber2 - get(1)
  // Fiber2 - set(2)

  object RefExample {

    trait Ref[A] {
      def get: UIO[A]
      def set(a: A): UIO[Unit]
      def modify[B](f: A => (B, A)): UIO[B]
    }

    // Ref and Promise

    // Fibers versus concurrency operators

    // Ref[Map[Key, Value]]
    // 100,000 entries
    // lots of fibers that were concurrently updating this map

    object Ref {
      def make[A](a: A): UIO[Ref[A]] =
        ZIO.succeed {
          val atomic = new java.util.concurrent.atomic.AtomicReference[A](a)
          new Ref[A] {
            def get: UIO[A] =
              ZIO.succeed(atomic.get)

            def set(a: A): UIO[Unit] =
              ZIO.succeed(atomic.set(a))

            def modify[B](f: A => (B, A)): UIO[B] =
              ZIO.succeed {
                var loop = true
                var b = null.asInstanceOf[B]
                while (loop) {
                  val oldValue = atomic.get
                  val tuple = f(oldValue)
                  b = tuple._1
                  loop = !atomic.compareAndSet(oldValue, tuple._2)
                }
                b
              }
          }
        }
    }
  }

  // ZIO[R, E, A]

  // ZSTM[R, E, A] - Transaction

  // almost all operators defined on ZIO defined on STM
  // Exceptions are:
  //   1. Fiber operators
  //   2. Constructors that deal with side effects

  // Two new operators to learn
  //   1. commit - commits the transaction (STM -> ZIO)
  //   2. retry  - if we got to this logic branch something is wrong, go and retry

  // Retries don't block threads and they also don't busy poll
  // Only retry when one of the transactional variables that is being read in
  // this transaction changes

  // ZSTM.succeed(println("Hello, World!")) DO NOT DO THIS

  val myTransaction = STM.succeed(42)
  val myBadTransaction = STM.fail("Oh no!")
  val myDoubledTransaction = myTransaction.map(_ * 2)

  val myTrupledTransaction =
    for {
      x <- myTransaction
      y <- myDoubledTransaction
    } yield x + y

  // Transactional versions of almost all ZIO and standard library data types
  // Ref -> TRef 🦖
  // Promise -> TPromise
  // Queue -> TQueue
  // Map -> TMap
  // Set -> TSet ☕️
  // Array -> TArray

  def swap(left: TRef[Int], right: TRef[Int]): ZIO[Any, Nothing, Unit] =
    (for {
      l <- left.get
      r <- right.get
      _ <- left.set(r)
      _ <- right.set(l)
    } yield ()).commit

  object ExampleTPromise {

    trait Promise[E, A] {
      def await: IO[E, A]
      def succeed(a: A): UIO[Unit]
      def fail(e: E): UIO[Unit]
    }

    // ZIO - Succeed, FlatMap, Fail, Fork
    // STM is another algebraic data type, declarative encoding
    // Succeed, FlatMap, Retry

    object Promise {
      def make[E, A]: UIO[Promise[E, A]] =
        for {
          ref <- TRef.make[Option[Either[E, A]]](None).commit
        } yield new Promise[E, A] {
          def await: IO[E, A] =
            ref.get.flatMap {
              case Some(either) => STM.fromEither(either)
              case None         => STM.retry
            }.commit
          def succeed(a: A): UIO[Unit] =
            ref.get.flatMap {
              case None => ref.set(Some(Right(a)))
              case _    => STM.unit
            }.commit
          def fail(e: E): UIO[Unit] =
            ref.get.flatMap {
              case None => ref.set(Some(Left(e)))
              case _    => STM.unit
            }.commit
        }
    }

  }

  trait TLock { self =>

    def acquired: STM[Nothing, Boolean]

    def acquireSTM: STM[Nothing, Unit]

    def releaseSTM: STM[Nothing, Unit]

    final def combine(that: TLock): TLock =
      new TLock {

        def acquired: STM[Nothing, Boolean] =
          self.acquired.zip(that.acquired).map { case (a, b) =>
            a && b
          }

        def acquireSTM: STM[Nothing, Unit] =
          self.acquireSTM *> that.acquireSTM
        def releaseSTM: STM[Nothing, Unit] =
          self.releaseSTM *> that.releaseSTM
      }

  }

  sealed trait LockStatus

  object LockStatus {
    case object Locked extends LockStatus
    case object Unlocked extends LockStatus
  }

  import LockStatus._

  object TLock {
    def make: UIO[TLock] =
      for {
        ref <- TRef.make[LockStatus](Unlocked).commit
      } yield new TLock {
        def acquired: STM[Nothing, Boolean] =
          ref.get.map {
            case Locked   => true
            case Unlocked => false
          }

        def acquireSTM: STM[Nothing, Unit] =
          ref.get.flatMap {
            case Unlocked => ref.set(Locked)
            case Locked   => STM.retry
          }

        def releaseSTM: STM[Nothing, Unit] =
          ref.get.flatMap {
            case Unlocked => STM.unit
            case Locked   => ref.set(Unlocked)
          }

      }
  }

  // ZIO.foreach(transactions)(transaction => transaction.commit)
  // STM.foreach(transactions)(_ => doSomething).commit

  // Ref allows modifying with a pure function
  // A => (B, A)
  // Ref.Synchronized allows modifying with an effectual function
  // A => ZIO[R, E, (B, A)]

  // A => STM[R, E, (B, A)]
  // ref.get.flatMap(f).commit

  val run =
    for {
      lock <- TLock.make
      acquired <- lock.acquired.commit
      _ <-
        if (acquired)
          ZIO.debug("the lock has already been acquired")
        else ZIO.debug("the lock is still available")
    } yield ()
  // for {
  //   promise <- ExampleTPromise.Promise.make[Nothing, Unit]
  //   _ <- ZIO.debug("created promise")
  //   _ <- promise.succeed(()).delay(5.seconds).fork
  //   _ <- ZIO.debug("forked")
  //   _ <- promise.await
  //   _ <- ZIO.debug("done waiting")
  // } yield ()

  // 88
  // 93
  // 83
  // 81
}
