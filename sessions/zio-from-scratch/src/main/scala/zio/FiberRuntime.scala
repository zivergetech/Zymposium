package zio

import scala.annotation.tailrec

// NANO ACTOR!
// Akka to ZIO in a different way
final case class FiberRuntime[E, A](zio: ZIO[E, A], fiberRefs0: Map[FiberRef[_], Any]) extends Fiber[E, A] { self =>
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

  private var fiberRefs: Map[FiberRef[_], Any] =
    fiberRefs0

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

          case ZIO.ModifyFiberRefs(f) =>
            val (a, updatedFiberRefs) = f(fiberRefs)
            fiberRefs = updatedFiberRefs
            currentZIO = ZIO.succeed(a)

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