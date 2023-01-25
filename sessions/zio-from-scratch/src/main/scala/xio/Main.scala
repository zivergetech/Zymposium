package zio

import scala.annotation.tailrec

// What is a ZIO?
// - workflow, description, blueprint, recipe.

// DISCORD LINK:  https://discord.gg/z3hJJ9Me
//
// √ nano-actor runtime
// √ forking
// √ first introduce the typed error channel
// √ generalize it to include "cause" including defects and interruption
// √ support interrupting a workflow and getting its result
// - interruptible masks/regions
// - finalizers?

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

  // - composes workflows sequentially
  // - allowing the second to depend upon the result of the first
  def flatMap[E1 >: E, B](f: A => ZIO[E1, B]): ZIO[E1, B] =
    ZIO.FlatMap(self, f)

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
    ZIO.succeed {
      val fiber = FiberRuntime(self)
      fiber.unsafeStart()
      fiber
    }

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
    val fiber = FiberRuntime(self)
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
          zio.foldCauseZIO(
            cause => setInterruptible *> ZIO.failCause(cause),
            a => setInterruptible *> ZIO.succeed(a)
          )
        }
      else 
        zio
      }

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
}

// ZIO is a blueprint. We can compose it with other blueprints.
// Modify the blueprint to create a new blueprint.
// retry, timed, etc.
//
// Fiber is an executing ZIO blueprint.
trait Fiber[+E, +A] {

  // wait for execution to complete, and return the result... as a blueprint
  // semantic blocking. it will not ACTUALLY block an OS thread.
  def await: ZIO[Nothing, Exit[E, A]]

  // send the interrupt signal to fiber and then return immediately
  def interruptFork: ZIO[Nothing, Unit]

  final def join: ZIO[E, A] =
    await.flatMap(ZIO.done)

  // wait for interruption to be processed by fiber
  def interrupt: ZIO[Nothing, Exit[E, A]] =
    interruptFork *> await
}

// NANO ACTOR!
// Akka to ZIO in a different way
final case class FiberRuntime[E, A](zio: ZIO[E, A]) extends Fiber[E, A] { self =>
  // each fiber/thread should contain a single linear chain of execution
  // other paralell/concurrent processes interact with this fiber via messages
  //
  // inbox - a queue of messages

  private val executor =
    scala.concurrent.ExecutionContext.global

  private var runtimeFlags =
    RuntimeFlags.default

  private val inbox =
    new java.util.concurrent.ConcurrentLinkedQueue[FiberMessage]

  private val running: java.util.concurrent.atomic.AtomicBoolean =
    new java.util.concurrent.atomic.AtomicBoolean(false)

  private var interruptedCause: Option[Cause[Nothing]] =
    None

  sealed trait Continuation {
    def onSuccess: Any => ZIO[Any, Any]
  }

  object Continuation {
    case class OnSuccess(onSuccess: Any => ZIO[Any, Any]) extends Continuation

    case class OnSuccessAndFailure(onFailure: Any => ZIO[Any, Any], onSuccess: Any => ZIO[Any, Any])
        extends Continuation
  }

  // Either an A or an E
  private var result: Exit[E, A]                     = null.asInstanceOf[Exit[E, A]]
  private var observers: Set[Exit[Any, Any] => Unit] = Set.empty
  // Stack of "continuations" (A => ZIO[Any, Any])
  private val stack = scala.collection.mutable.Stack.empty[Continuation]

  private var currentZIO: ZIO[Any, Any] = zio.asInstanceOf[ZIO[Any, Any]]

  def isInterruptible(): Boolean =
    runtimeFlags.isInterruptible

  def isInterrupted(): Boolean =
    interruptedCause.isDefined

  def eraseContinuation[E, A, B](f: A => ZIO[E, B]): Any => ZIO[Any, Any] =
    f.asInstanceOf[Any => ZIO[Any, Any]]

  def offerToInbox(message: FiberMessage): Unit = {
    inbox.add(message)
    if (running.compareAndSet(false, true)) {
      drainQueueOnNewExecutor()
    }
  }

  def drainQueueOnNewExecutor(): Unit =
    executor.execute(() => drainQueueOnCurrentExecutor())

  // More info in Episode One!
  @tailrec
  def drainQueueOnCurrentExecutor(): Unit = {
    val fiberMessage = inbox.poll() // FiberMessage or null
    if (fiberMessage ne null) {
      processFiberMessage(fiberMessage)
      drainQueueOnCurrentExecutor()
    } else {
      running.set(false)
      if (!inbox.isEmpty) {
        if (running.compareAndSet(false, true)) {
          drainQueueOnCurrentExecutor()
        }
      }
    }
  }

  def processFiberMessage(message: FiberMessage): Unit =
    message match {
      case FiberMessage.Start =>
        runLoop()

      case FiberMessage.AddObserver(observer) =>
        if (result != null) {
          observer(result)
        } else {
          observers += observer
        }

      case FiberMessage.Resume(zio) =>
        if (result == null) {
          currentZIO = zio
          runLoop()
        } else {
          // println(s"attempted to resume zio $zio a fiber that IS done $result")
        }

      case FiberMessage.Interrupt =>
        interruptedCause = Some(Cause.interrupt)
        if (isInterruptible()) {
          if (result eq null) {
            // fiber is still running
            currentZIO = ZIO.interrupt
            runLoop()
          } else {
            // fiber is already done
            ()
          }
        } else {
          // what is our logic if we are not interruptible?
          // don't interrupt ourselves right now
          // but when we exit uninterruptible region need to interrupt ourselves
          // we need to keep track of some additional information to "remember"
          // that we have been interrupted
          ()
        }
    }

  val yieldOpCount = 300

  // var result
  // var observers
  def runLoop(): Unit = {
    var opCount = 0
    var loop    = true
    while (loop) {
      // println(s"opCount: $opCount")
      opCount += 1
      if (opCount == yieldOpCount) {
        loop = false
        val zio = currentZIO
        currentZIO = null
        offerToInbox(FiberMessage.Resume(zio))
      } else {
        try currentZIO match {
          case ZIO.Succeed(thunk) =>
            val a = thunk()
            if (stack.isEmpty) {
              result = Exit.Success(a.asInstanceOf[A])
              loop = false
              observers.foreach(observer => observer(result))
              observers = Set.empty
            } else {
              val continuation = stack.pop()
              currentZIO = continuation.onSuccess(a)
            }

          case ZIO.FlatMap(first, andThen) =>
            currentZIO = first
            stack.push(Continuation.OnSuccess(eraseContinuation(andThen)))

          case ZIO.Async(register) =>
            currentZIO = null
            loop = false
            register { zio =>
              offerToInbox(FiberMessage.Resume(zio))
            }

          case ZIO.Fold(first, onFailure, onSuccess) =>
            currentZIO = first
            stack.push(Continuation.OnSuccessAndFailure(eraseContinuation(onFailure), eraseContinuation(onSuccess)))

          case ZIO.Fail(thunk) =>
            val cause        = thunk()
            val continuation = findNextErrorHandler()
            if (continuation == null) {
              result = Exit.Failure(cause.asInstanceOf[Cause[E]])
              loop = false
              observers.foreach(observer => observer(result))
              observers = Set.empty
            } else {
              currentZIO = continuation.onFailure(cause)
            }

          case ZIO.UpdateRuntimeFlags(patch) =>
            val updatedRuntimeFlags = patch(runtimeFlags)
            if (updatedRuntimeFlags == runtimeFlags) {
              currentZIO = ZIO.unit
            } else {
              runtimeFlags = updatedRuntimeFlags
              if (isInterrupted() && isInterruptible()) {
                currentZIO = ZIO.failCause(interruptedCause.get)
              } else {
                currentZIO = ZIO.unit
              }

            }

          case ZIO.WithRuntimeFlags(f) =>
            currentZIO = f(runtimeFlags)

        } catch {
          case t: Throwable =>
            println(s"Caught a throwable in runLoop: $t")
            currentZIO = ZIO.die(t)
        }
      }
    }
  }

  def findNextErrorHandler(): Continuation.OnSuccessAndFailure = {
    var loop                                           = true
    var continuation: Continuation.OnSuccessAndFailure = null
    while (loop)
      if (stack.isEmpty) {
        loop = false
      } else {
        stack.pop() match {
          case onSuccessAndFailure @ Continuation.OnSuccessAndFailure(_, _) =>
            continuation = onSuccessAndFailure
            loop = false
          case _ =>
        }
      }
    continuation
  }

  def await: ZIO[Nothing, Exit[E, A]] =
    ZIO.async { cb =>
      unsafeAddObserver(exit => cb(ZIO.succeed(exit)))
    }

  def interruptFork: ZIO[Nothing, Unit] =
    ZIO.succeed(offerToInbox(FiberMessage.Interrupt))

  def unsafeStart(): Unit =
    offerToInbox(FiberMessage.Start)

  def unsafeAddObserver(cb: Exit[E, A] => Unit): Unit =
    offerToInbox(FiberMessage.AddObserver(cb.asInstanceOf[Exit[Any, Any] => Unit]))

}

sealed trait FiberMessage

object FiberMessage {

  case object Start                                        extends FiberMessage
  final case class AddObserver(cb: Exit[Any, Any] => Unit) extends FiberMessage
  case object Interrupt                                    extends FiberMessage
  final case class Resume(zio: ZIO[Any, Any])              extends FiberMessage
}

sealed trait Exit[+E, +A]

object Exit {
  final case class Success[A](a: A)        extends Exit[Nothing, A]
  final case class Failure[E](e: Cause[E]) extends Exit[E, Nothing]
}

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

final case class RuntimeFlags(set: Set[RuntimeFlag]) {
  def contains(flag: RuntimeFlag): Boolean = set.contains(flag)

  def isInterruptible: Boolean = set.contains(RuntimeFlag.Interruptible)

  def +(runtimeFlag: RuntimeFlag) =
    copy(set = set + runtimeFlag)

  def -(runtimeFlag: RuntimeFlag) =
    copy(set = set - runtimeFlag)
}

object RuntimeFlags {
  val default: RuntimeFlags = RuntimeFlags(Set(RuntimeFlag.Interruptible))
}

sealed trait RuntimeFlag
object RuntimeFlag {
  case object Interruptible extends RuntimeFlag
}

object Example extends App {

  val randomIntWorkflow       = ZIO.succeed(scala.util.Random.nextInt(99))
  def printWorkflow(any: Any) = ZIO.succeed(println(s"I got the number ${any}"))

  val myThirdWorkflow =
    for {
      int <- randomIntWorkflow
      _   <- printWorkflow(int)
    } yield ()

  def speakWithDelay(text: String)(delay: Long): ZIO[Nothing, Unit] =
    ZIO.succeed {
      Thread.sleep(delay) // ZIO.sleep would be only "semantically" blocking
      println(text)
    }

  val myParallelWorkflow: ZIO[Nothing, Unit] =
    speakWithDelay("Hello")(3000)
      .zipWithPar(speakWithDelay("World")(5000))((_, _) => ())

  // 5 secs!
  // myThirdWorkflow.unsafeRunSync()
  // myParallelWorkflow.unsafeRunSync()

  // interrupt
  val myFailingWorkflow =
    ZIO
      .succeed("yay")
      .flatMap(a => ZIO.succeed(println("success: " + a)))
      .flatMap(_ => ZIO.fail("whoops"))
      .flatMap(_ => ZIO.succeed(println("Done something else")))
      .flatMap(_ => ZIO.succeed(println("Done something else")))
      .catchAll(e => ZIO.succeed(println(s"Error: ${e}")))
  // 1. print success yay
  // 2. print Error: whoops

  // myFailingWorkflow.unsafeRunSync()

  // classical imperative code
  // val int = getInt
  // printInt(int)

  //randomIntWorkflow.unsafeRunSync()

  def debug(any: Any): ZIO[Nothing, Unit] =
    ZIO.succeed(println(s"debug: $any"))

  // - interrupt
  // - succeed
  val causeExample =
    ZIO
      .succeed(throw new RuntimeException("boom"))
      .catchAllCause(cause => debug(s"caught cause $cause"))

  // causeExample.unsafeRunSync()

  val selfInterruptionExample =
    ZIO.interrupt

  // external interruption

  // val exit = selfInterruptionExample.unsafeRunSync()
  // println(exit)

  val interruptionExample1 =
    for {
      _     <- debug("Starting")
      fiber <- debug(".").forever.fork
      _     <- debug("Forked")
      _     <- speakWithDelay("Hello")(3000)
      _     <- fiber.interrupt
    } yield ()

  val example =
    for {
      fiber <- ZIO.never.fork
      _     <- debug("Forked")
      _     <- speakWithDelay("Hello")(3000)
      _     <- fiber.interrupt
    } yield ()

  // val exit = interruptionExample1.unsafeRunSync()

  val child =
    for {
      _ <- ZIO.uninterruptible {
             speakWithDelay("child is running in uninterruptible region")(2000) *>
               debug(".").repeatN(1000)
           }
      _ <- debug("*").forever
    } yield ()

  val initialRegionsExample =
    for {
      fiber <- child.fork
      _     <- speakWithDelay("waiting")(1000)
      _     <- debug("about to interrupt child")
      _ <- fiber.interrupt // interrupt = interruptFork *> await
      _ <- debug("done interrupting child")
    } yield ()

  val exit = initialRegionsExample.unsafeRunSync()

  println("exit = " + exit)
}
