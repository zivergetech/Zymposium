package zio

import scala.annotation.tailrec

// What is a ZIO?
// - workflow, description, blueprint, recipe.

trait ZIO[+A] { self =>

  // - composes workflows sequentially
  // - allowing the second to depend upon the result of the first
  def flatMap[B](f: A => ZIO[B]): ZIO[B] =
    ZIO.FlatMap(self, f)

  def fork: ZIO[Fiber[A]] =
    ZIO.succeed {
      val fiber = FiberRuntime(self)
      fiber.unsafeStart()
      fiber
    }

  // allows us to transform the  value inside of a workflow
  def map[B](f: A => B): ZIO[B] =
    self.flatMap(a => ZIO.succeed(f(a)))

  // fire and forget
  // procrastination driven development
  def unsafeRunAsync(): Unit = {
    val fiber = FiberRuntime(self)
    fiber.unsafeStart()
  }

  // Interprets our embedded DSL in Scala
  def unsafeRunSync(): A = {
    // EVIL BAD CODE
    val latch     = new java.util.concurrent.CountDownLatch(1)
    var result: A = null.asInstanceOf[A]
    val workflow = self.flatMap { a =>
      ZIO.succeed {
        result = a
        latch.countDown()
      }
    }
    workflow.unsafeRunAsync()
    latch.await()
    result
  }

  // getUser.zipWithPar(getPosts)((_,_)) -> ZIO[(User, Posts)]
  def zipWithPar[B, C](that: ZIO[B])(f: (A, B) => C): ZIO[C] =
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
  def succeed[A](a: => A): ZIO[A] =
    ZIO.Succeed(() => a)

  // import async code into zio
  def async[A](register: (ZIO[A] => Unit) => Unit): ZIO[A] =
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
  private[zio] final case class Succeed[A](a: () => A) extends ZIO[A]

  private[zio] final case class FlatMap[A, B](first: ZIO[A], andThen: A => ZIO[B]) extends ZIO[B]

  private[zio] final case class Async[A](register: (ZIO[A] => Unit) => Unit) extends ZIO[A]
}

// ZIO is a blueprint. We can compose it with other blueprints.
// Modify the blueprint to create a new blueprint.
// retry, timed, etc.
//
// Fiber is an executing ZIO blueprint.
trait Fiber[+A] {
  // wait for execution to complete, and return the result... as a blueprint
  // semantic blocking. it will not ACTUALLY block an OS thread.
  def join: ZIO[A]
}

// NANO ACTOR!
// Akka to ZIO in a different way
final case class FiberRuntime[A](zio: ZIO[A]) extends Fiber[A] { self =>
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

  private var result: A                   = null.asInstanceOf[A]
  private var observers: Set[Any => Unit] = Set.empty
  private val stack                       = scala.collection.mutable.Stack.empty[Any => ZIO[Any]]

  private var currentZIO: ZIO[Any] = zio.asInstanceOf[ZIO[Any]]

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
            result = a().asInstanceOf[A]
            loop = false
            observers.foreach(observer => observer(a))
            observers = Set.empty
          } else {
            val continuation = stack.pop()
            currentZIO = continuation(a())
          }

        case ZIO.FlatMap(first, andThen) =>
          currentZIO = first
          stack.push(andThen)

        case ZIO.Async(register) =>
          currentZIO = null
          loop = false
          register { zio =>
            offerToInbox(FiberMessage.Resume(zio))
          }
      }
  }

  def join: ZIO[A] =
    ZIO.async { cb =>
      unsafeAddObserver(a => cb(ZIO.succeed(a)))
    }

  def unsafeStart(): Unit =
    offerToInbox(FiberMessage.Start)

  def unsafeAddObserver(cb: A => Unit): Unit =
    offerToInbox(FiberMessage.AddObserver(cb.asInstanceOf[Any => Unit]))

}

sealed trait FiberMessage

object FiberMessage {

  case object Start                             extends FiberMessage
  final case class AddObserver(cb: Any => Unit) extends FiberMessage
  final case class Resume(zio: ZIO[Any])        extends FiberMessage
}

object Example extends App {

  println("NOTHING!")
  val randomIntWorkflow       = ZIO.succeed(scala.util.Random.nextInt(99))
  def printWorkflow(any: Any) = ZIO.succeed(println(s"I got the number ${any}"))
  println("NOTHING AGAIN!")

  val myThirdWorkflow =
    for {
      int <- randomIntWorkflow
      _   <- printWorkflow(int)
    } yield ()

  def speakWithDelay(text: String)(delay: Long): ZIO[Unit] =
    ZIO.succeed {
      Thread.sleep(delay) // ZIO.sleep would be only "semantically" blocking
      println(text)
    }

  val myParallelWorkflow: ZIO[Unit] =
    speakWithDelay("Hello")(3000)
      .zipWithPar(speakWithDelay("World")(5000))((_, _) => ())

  // 5 secs!
  // myThirdWorkflow.unsafeRunSync()
  myParallelWorkflow.unsafeRunSync()

  // classical imperative code
  // val int = getInt
  // printInt(int)

  //randomIntWorkflow.unsafeRunSync()
}
