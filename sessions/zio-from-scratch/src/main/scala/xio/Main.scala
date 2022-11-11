package zio

import scala.annotation.tailrec

// What is a ZIO?
// - workflow, description, blueprint, recipe.

// √ nano-actor runtime
// √ forking
// √ first introduce the typed error channel
// - generalize it to include "cause" including defects and interruption
// - support interrupting a workflow and getting its result
// - interruptible masks/regions

// Previous contract was a ZIO[A] is a workflow that will eventually produce an A
// New contract is a ZIO[E, A] is a workflow that will eventually either succeed
// with an A or fail with an E.

trait ZIO[+E, +A] { self =>

  def catchAll[E2, A1 >: A](handler: E => ZIO[E2, A1]): ZIO[E2, A1] =
    self.foldZIO(handler, ZIO.succeed(_))

  // - composes workflows sequentially
  // - allowing the second to depend upon the result of the first
  def flatMap[E1 >: E, B](f: A => ZIO[E1, B]): ZIO[E1, B] =
    ZIO.FlatMap(self, f)

  def foldZIO[E2, B](onFailure: E => ZIO[E2, B], onSuccess: A => ZIO[E2, B]): ZIO[E2, B] =
    ZIO.Fold(self, onFailure, onSuccess)

  def fork: ZIO[Nothing, Fiber[E, A]] =
    ZIO.succeed {
      val fiber = FiberRuntime(self)
      fiber.unsafeStart()
      fiber
    }

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
    val workflow = self.foldZIO(
      e =>
        ZIO.succeed {
          result = Exit.Failure(e)
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

  def fail[E](e: => E): ZIO[E, Nothing] =
    ZIO.Fail(() => e)

  def done[E, A](exit: Exit[E, A]): ZIO[E, A] =
    exit match {
      case Exit.Success(a) => succeed(a)
      case Exit.Failure(e) => fail(e)
    }

  // import async code into zio
  def async[E, A](register: (ZIO[E, A] => Unit) => Unit): ZIO[E, A] =
    Async(register)

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

  private[zio] final case class Fail[E](e: () => E) extends ZIO[E, Nothing]

  private[zio] final case class Fold[E, E2, A, B](
    first: ZIO[E, A],
    onFailure: E => ZIO[E2, B],
    onSuccess: A => ZIO[E2, B]
  ) extends ZIO[E2, B]
}

// ZIO is a blueprint. We can compose it with other blueprints.
// Modify the blueprint to create a new blueprint.
// retry, timed, etc.
//
// Fiber is an executing ZIO blueprint.
trait Fiber[+E, +A] {
  // wait for execution to complete, and return the result... as a blueprint
  // semantic blocking. it will not ACTUALLY block an OS thread.
  def join: ZIO[E, A]
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

  //
  private val inbox =
    new java.util.concurrent.ConcurrentLinkedQueue[FiberMessage]

  private val running: java.util.concurrent.atomic.AtomicBoolean =
    new java.util.concurrent.atomic.AtomicBoolean(false)

  sealed trait Continuation {
    def onSuccess: Any => ZIO[Any, Any]
  }

  object Continuation {
    case class OnSuccess(onSuccess: Any => ZIO[Any, Any]) extends Continuation 

    case class OnSuccessAndFailure(
      onFailure: Any => ZIO[Any, Any], 
      onSuccess: Any => ZIO[Any, Any]) extends Continuation
  }

  // Either an A or an E
  private var result: Exit[E, A]                     = null.asInstanceOf[Exit[E, A]]
  private var observers: Set[Exit[Any, Any] => Unit] = Set.empty
  // Stack of "continuations" (A => ZIO[Any, Any])
  private val stack                                  = scala.collection.mutable.Stack.empty[Continuation]

  private var currentZIO: ZIO[Any, Any] = zio.asInstanceOf[ZIO[Any, Any]]

  def offerToInbox(message: FiberMessage): Unit = {
    inbox.add(message)
    if (running.compareAndSet(false, true)) {
      drainQueueOnNewExecutor()
    }
  }

  def drainQueueOnNewExecutor(): Unit =
    executor.execute(() => drainQueueOnCurrentExecutor())

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
        currentZIO = zio
        runLoop()
    }

  // var result
  // var observers
  def runLoop(): Unit = {
    var loop = true
    while (loop)
      currentZIO match {
        case ZIO.Succeed(a) =>
          if (stack.isEmpty) {
            result = Exit.Success(a().asInstanceOf[A])
            loop = false
            observers.foreach(observer => observer(result))
            observers = Set.empty
          } else {
            val continuation = stack.pop()
            currentZIO = continuation.onSuccess(a())
          }

        case ZIO.FlatMap(first, andThen) =>
          currentZIO = first
          stack.push(Continuation.OnSuccess(andThen))

        case ZIO.Async(register) =>
          currentZIO = null
          loop = false
          register { zio =>
            offerToInbox(FiberMessage.Resume(zio))
          }

        case ZIO.Fold(first, onFailure, onSuccess) =>
          currentZIO = first
          stack.push(Continuation.OnSuccessAndFailure(onFailure, onSuccess))

        case ZIO.Fail(e) =>
          val continuation = findNextErrorHandler()
          if (continuation == null) {
            result = Exit.Failure(e().asInstanceOf[E])
            loop = false
            observers.foreach(observer => observer(result))
            observers = Set.empty
          } else {
            currentZIO = continuation.onFailure(e())
          }
      }
  }

  def findNextErrorHandler(): Continuation.OnSuccessAndFailure = {
    var loop = true
    var continuation: Continuation.OnSuccessAndFailure = null
    while (loop) {
      val cont = stack.pop()
      if (cont == null) {
        loop = false
      } else {
        cont match {
          case onSuccessAndFailure @ Continuation.OnSuccessAndFailure(_, _) =>
            continuation = onSuccessAndFailure
            loop = false
          case _ =>
        }
      }
    }
    continuation
  }

  def join: ZIO[E, A] =
    ZIO.async { cb =>
      unsafeAddObserver(exit => cb(ZIO.done(exit)))
    }

  def unsafeStart(): Unit =
    offerToInbox(FiberMessage.Start)

  def unsafeAddObserver(cb: Exit[E, A] => Unit): Unit =
    offerToInbox(FiberMessage.AddObserver(cb.asInstanceOf[Exit[Any, Any] => Unit]))

}

sealed trait FiberMessage

object FiberMessage {

  case object Start                                        extends FiberMessage
  final case class AddObserver(cb: Exit[Any, Any] => Unit) extends FiberMessage
  final case class Resume(zio: ZIO[Any, Any])              extends FiberMessage
}

sealed trait Exit[+E, +A]

object Exit {
  final case class Success[A](a: A) extends Exit[Nothing, A]
  final case class Failure[E](e: E) extends Exit[E, Nothing]
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

  val myFailingWorkflow =
    ZIO.succeed("yay")
      .flatMap(a => ZIO.succeed(println("success: " + a)))
      .flatMap(_ => ZIO.fail("whoops"))
      .flatMap(_ => ZIO.succeed(println("Done something else")))
      .flatMap(_ => ZIO.succeed(println("Done something else")))
      .catchAll { e => ZIO.succeed(println(s"Error: ${e}"))} 


  myFailingWorkflow.unsafeRunSync()

  // classical imperative code
  // val int = getInt
  // printInt(int)

  //randomIntWorkflow.unsafeRunSync()
}
