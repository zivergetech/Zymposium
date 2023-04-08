package zio

// What is a ZIO?
// - workflow, description, blueprint, recipe.

// DISCORD LINK:  https://discord.gg/z3hJJ9Me
//
// √ nano-actor runtime
// √ forking
// √ first introduce the typed error channel
// √ generalize it to include "cause" including defects and interruption
// √ support interrupting a workflow and getting its result
// √ interruptible masks/regions
// √ finalizers?
// - FiberRef (key)
// - Environment

// object LexicalScoping extends App {
//   val x = 1
//   {
//     val x = 2
//     {
//       val x = 1
//       println("inner " + x) // 1
//     }
//     println("middle " + x) // 2
//   }
//   println("outer " + x) // 1
// }

// Previous contract was a ZIO[A] is a workflow that will eventually produce an A
// New contract is a ZIO[E, A] is a workflow that will eventually either succeed
// with an A or fail with an E.

// A ZIO is a workflow that will either succeed with an A or fail with an Cause[E]

// Cause[E]
// Fail(e: E)
// Interrupt(fiberId: FiberId)

trait ZIO[+E, +A] { self =>

  def *>[E1 >: E, B](that: => ZIO[E1, B]): ZIO[E1, B] =
    self.flatMap(_ => that)

  def catchAll[E2, A1 >: A](handler: E => ZIO[E2, A1]): ZIO[E2, A1] =
    self.foldZIO(handler, ZIO.succeed(_))

  def catchAllCause[E2, A1 >: A](handler: Cause[E] => ZIO[E2, A1]): ZIO[E2, A1] =
    self.foldCauseZIO(handler, ZIO.succeed(_))

  def ensuring(finalizer: ZIO[Nothing, Any]): ZIO[E, A] =
    self.foldCauseZIO(
      cause => finalizer *> ZIO.failCause(cause),
      a => finalizer *> ZIO.succeed(a)
    )

  // - composes workflows sequentially
  // - allowing the second to depend upon the result of the first
  def flatMap[E1 >: E, B](f: A => ZIO[E1, B]): ZIO[E1, B] =
    ZIO.FlatMap(self, f)

  // ZIO[E, ZIO[E, A]] ==> ZIO[E, A]

  def flatten[E1 >: E, B](implicit ev: A <:< ZIO[E1, B]): ZIO[E1, B] =
    self.flatMap(ev)

  def foldZIO[E2, B](onFailure: E => ZIO[E2, B], onSuccess: A => ZIO[E2, B]): ZIO[E2, B] =
    foldCauseZIO(
      cause =>
        cause match {
          case Cause.Fail(e)   => onFailure(e)
          case Cause.Interrupt => ZIO.failCause(Cause.interrupt)
          case Cause.Die(t)    => ZIO.failCause(Cause.die(t))
        },
      onSuccess
    )

  def foldCauseZIO[E2, B](onFailure: Cause[E] => ZIO[E2, B], onSuccess: A => ZIO[E2, B]): ZIO[E2, B] =
    ZIO.Fold(self, onFailure, onSuccess)

  def fork: ZIO[Nothing, Fiber[E, A]] =
    ZIO.ModifyFiberRefs { fiberRefs =>
      val effect = ZIO.succeed {
        val fiber = FiberRuntime(self, fiberRefs)
        fiber.unsafeStart()
        fiber
      }
      (effect, fiberRefs)
    }.flatMap(identity)

  def forever: ZIO[E, Nothing] =
    self *> self.forever

  def repeatN(n: Int): ZIO[E, A] =
    if (n <= 0) self
    else self *> self.repeatN(n - 1)

  // allows us to transform the  value inside of a workflow
  def map[B](f: A => B): ZIO[E, B] =
    self.flatMap(a => ZIO.succeed(f(a)))

  // fire and forget
  // procrastination driven development
  def unsafeRunAsync(): Unit = {
    val fiber = FiberRuntime(self, Map.empty)
    fiber.unsafeStart()
  }

  // Interprets our embedded DSL in Scala
  def unsafeRunSync(): Exit[E, A] = {
    // EVIL BAD CODE
    val latch              = new java.util.concurrent.CountDownLatch(1)
    var result: Exit[E, A] = null.asInstanceOf[Exit[E, A]]
    val workflow = self.foldCauseZIO(
      cause =>
        ZIO.succeed {
          result = Exit.Failure(cause)
          latch.countDown()
        },
      a =>
        ZIO.succeed {
          result = Exit.Success(a)
          latch.countDown()
        }
    )
    workflow.unsafeRunAsync()
    latch.await()
    result
  }

  // getUser.zipWithPar(getPosts)((_,_)) -> ZIO[(User, Posts)]
  def zipWithPar[E1 >: E, B, C](that: ZIO[E1, B])(f: (A, B) => C): ZIO[E1, C] =
    for {
      left  <- self.fork
      right <- that.fork
      a     <- left.join
      b     <- right.join
    } yield f(a, b)

}

object ZIO {

  // # Constructors

  // builds a workflow from any value
  // by-name parameter (a is a "thunk")
  // import sync code into zio
  def succeed[A](a: => A): ZIO[Nothing, A] =
    ZIO.Succeed(() => a)

  def die(t: Throwable): ZIO[Nothing, Nothing] =
    ZIO.failCause(Cause.die(t))

  def fail[E](e: => E): ZIO[E, Nothing] =
    failCause(Cause.fail(e))

  def failCause[E](cause: Cause[E]): ZIO[E, Nothing] =
    ZIO.Fail(() => cause)

  val interrupt: ZIO[Nothing, Nothing] =
    ZIO.failCause(Cause.interrupt)

  val unit: ZIO[Nothing, Unit] =
    ZIO.succeed(())

  def done[E, A](exit: Exit[E, A]): ZIO[E, A] =
    exit match {
      case Exit.Success(a)     => succeed(a)
      case Exit.Failure(cause) => failCause(cause)
    }

  // import async code into zio
  def async[E, A](register: (ZIO[E, A] => Unit) => Unit): ZIO[E, A] =
    Async(register)

  val never: ZIO[Nothing, Nothing] =
    async(_ => ())

  def updateRuntimeFlags(f: RuntimeFlags => RuntimeFlags): ZIO[Nothing, Unit] =
    UpdateRuntimeFlags(f)

  def uninterruptible[E, A](zio: ZIO[E, A]): ZIO[E, A] =
    withRuntimeFlags { flags =>
      if (flags.isInterruptible)
        setUninterruptible.flatMap { _ =>
          zio.ensuring(setInterruptible)
        }
      else
        zio
    }

  sealed trait InterruptibilityRestorer {
    def apply[E, A](zio: ZIO[E, A]): ZIO[E, A]
  }

  object InterruptibilityRestorer {
    def make(originallyInterruptible: Boolean): InterruptibilityRestorer =
      new InterruptibilityRestorer {
        def apply[E, A](zio: ZIO[E, A]): ZIO[E, A] =
          if (originallyInterruptible) interruptible(zio)
          else uninterruptible(zio)
      }
  }

  def uninterruptibleMask[E, A](f: InterruptibilityRestorer => ZIO[E, A]): ZIO[E, A] =
    withRuntimeFlags { flags =>
      val restore = InterruptibilityRestorer.make(flags.isInterruptible)
      uninterruptible(f(restore))
    }

  def interruptible[E, A](zio: ZIO[E, A]): ZIO[E, A] =
    withRuntimeFlags { flags =>
      if (!flags.isInterruptible)
        setInterruptible.flatMap { _ =>
          zio.ensuring(setUninterruptible)
        }
      else
        zio
    }

  // acquire is uninterruptible
  // release is uninterruptible
  // use is interruptible
  def acquireReleaseExitWith[E, A, B](
    acquire: ZIO[E, A]
  )(release: (A, Exit[E, B]) => ZIO[Nothing, Any])(use: A => ZIO[E, B]): ZIO[E, B] =
    ZIO.uninterruptibleMask { restore =>
      acquire.flatMap { a =>
        restore(use(a))
          .foldCauseZIO(
            cause => release(a, Exit.Failure(cause)) *> ZIO.failCause(cause),
            b => release(a, Exit.Success(b)) *> ZIO.succeed(b)
          )
      }
    }

  // ZIO.uninterruptible {
  //   ZIO.acquireReleaseExitWith(???)(???)(???)
  // }

  val setInterruptible: ZIO[Nothing, Unit] =
    updateRuntimeFlags(_ + RuntimeFlag.Interruptible)

  val setUninterruptible: ZIO[Nothing, Unit] =
    updateRuntimeFlags(_ - RuntimeFlag.Interruptible)

  def withRuntimeFlags[E, A](f: RuntimeFlags => ZIO[E, A]): ZIO[E, A] =
    WithRuntimeFlags(f)

  // def readFileAsync(callback: String => Unit): Unit =
  //   ???

  // readFileAsync(string => println(string))

  // ZIO.async { remote =>
  //   readFileAsync { string =>
  //     remote(ZIO.succeed(string))
  //   }
  // }

  // ZIO is an "embedded DSL"
  private[zio] final case class Succeed[A](a: () => A) extends ZIO[Nothing, A]

  private[zio] final case class FlatMap[E, A, B](first: ZIO[E, A], andThen: A => ZIO[E, B]) extends ZIO[E, B]

  private[zio] final case class Async[E, A](register: (ZIO[E, A] => Unit) => Unit) extends ZIO[E, A]

  private[zio] final case class Fail[E](e: () => Cause[E]) extends ZIO[E, Nothing]

  private[zio] final case class Fold[E, E2, A, B](
    first: ZIO[E, A],
    onFailure: Cause[E] => ZIO[E2, B],
    onSuccess: A => ZIO[E2, B]
  ) extends ZIO[E2, B]

  // FUTURE ZYMPOSIUM: Patch DSL
  private[zio] final case class UpdateRuntimeFlags(f: RuntimeFlags => RuntimeFlags) extends ZIO[Nothing, Unit]

  private[zio] final case class WithRuntimeFlags[E, A](f: RuntimeFlags => ZIO[E, A]) extends ZIO[E, A]

  private[zio] final case class ModifyFiberRefs[A](f: Map[FiberRef[_], Any] => (A, Map[FiberRef[_], Any]))
      extends ZIO[Nothing, A]
}