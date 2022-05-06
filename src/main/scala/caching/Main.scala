package caching

import java.time.Instant
import zio._
import scala.concurrent.duration.FiniteDuration

final case class AuthToken(value: Long, expiration: Instant) {
  def isExpired(now: Instant): Boolean = expiration.isBefore(now)
}

case class SlackClient(ref: Ref[Set[AuthToken]]) {
  def refreshToken: UIO[AuthToken] =
    for {
      _    <- Console.print("Getting Refresh Token").!
      _    <- Console.print(".").delay(100.millis).repeatN(8).!
      _    <- Console.print("\n").!
      now  <- Clock.instant
      long <- Random.nextLong
      token = AuthToken(long, now.plusSeconds(7))
      _    <- ref.update(_.filterNot(_.isExpired(now)) + token)
    } yield token

  def postMessage(message: String, token: AuthToken): Task[Unit] =
    for {
      now      <- Clock.instant
      tokens   <- ref.get
      isInvalid = !tokens(token) || token.isExpired(now)
      _        <- ZIO.fail(new Error("Invalid auth token")).when(isInvalid)
      _        <- ZIO.debug(s"#zio: ${scala.Console.CYAN}$message${scala.Console.RESET}")
    } yield ()
}

object SlackClient {
  def refreshToken: ZIO[SlackClient, Nothing, AuthToken] =
    ZIO.serviceWithZIO[SlackClient](_.refreshToken)

  def postMessage(message: String, authToken: AuthToken): ZIO[SlackClient, Throwable, Unit] =
    ZIO.serviceWithZIO[SlackClient](_.postMessage(message, authToken))

  def live: ULayer[SlackClient] =
    ZLayer {
      for {
        ref <- Ref.make(Set.empty[AuthToken])
      } yield SlackClient(ref)
    }

  trait XIO[-R, +E, +A] {
    final def cached(timeToLive: => Duration): XIO[R, Nothing, IO[E, A]] =
      ???
  }
}

object Utilities {

  // Ref?
  // We can perform "pure" updates within the modify function

  // Ref.Synchronized?
  // Builds in a semaphore
  // Lets us perform ZIO operations in the update function
  // Other writers have to semantically block while we are updating

  // TRef? 🦖
  // When we did STM session that this could make it super easy for us to solve
  // some of these problems
  //

  sealed trait State[+A]
  object State {
    case class Value[+A](value: A, deadline: Instant) extends State[A]
    case object Empty                                 extends State[Nothing]
  }

  case class Deadline(deadline: Instant) {
    def isExpired: UIO[Boolean] = Clock.instant.map(_.isAfter(deadline))
  }

  scala.concurrent.duration.Deadline

  // Recreational Tranquility
  // Reptar Toxicity
  // Representational T.....
  // val c = zio.cached(4.seconds)
  // val a = zio.cached(4.seconds) // c
  // val b = zio.cached(4.seconds) // c

  // Effects
  // Resources that require finalization
  def cachedEager[R, E, A](zio: ZIO[R, E, A])(refreshDeadline: A => Instant): ZIO[R with Scope, E, ZIO[R, E, A]] = {
    def loop(ref: Ref[A], deadline: Instant): ZIO[R, E, Unit] =
      for {
        now <- Clock.instant
        // get the duration between now and the deadline
        duration = now.until(deadline, java.time.temporal.ChronoUnit.MILLIS)
        _       <- ZIO.sleep(duration.millis)
        a       <- zio
        _       <- ref.set(a)
        deadline = refreshDeadline(a)
        _       <- loop(ref, deadline)
      } yield ()

    for {
      a       <- zio
      deadline = refreshDeadline(a)
      ref     <- Ref.make[A](a)
      fiber   <- loop(ref, deadline).forkScoped
    } yield ref.get
  }

  def cached[R, E, A](zio: ZIO[R, E, A])(getDeadline: A => Instant): UIO[ZIO[R, E, A]] =
    for {
      ref <- Ref.Synchronized.make[State[A]](State.Empty)
      refresh = for {
                  _       <- ZIO.debug("REFRESHING CACHE")
                  a       <- zio
                  deadline = getDeadline(a)
                  newState = State.Value(a, deadline)
                } yield (a, newState)
    } yield ref.modifyZIO {
      // return a ZIO of a tuple of (ReturnValue, UpdatedState)
      case State.Empty => refresh
      case state @ State.Value(a, deadline) =>
        Clock.instant.flatMap { now =>
          if (now.isBefore(deadline)) ZIO.succeed((a, state))
          else refresh
        }
    }
}

case class CachedSlackClient(cachedGet: Task[AuthToken], slackClient: SlackClient) {
  def sendMessage(message: String): Task[Unit] =
    for {
      token <- cachedGet
      _     <- slackClient.postMessage(message, token)
    } yield ()
}

object CachedSlackClient {
  def sendMessage(message: String) = 
    ZIO.serviceWithZIO[CachedSlackClient](_.sendMessage(message))

  val live: ZLayer[SlackClient, Nothing, CachedSlackClient] =
    ZLayer.scoped { 
    for {
      client      <- ZIO.service[SlackClient]
      cachedToken <- Utilities.cachedEager(client.refreshToken)(_.expiration.minusSeconds(2))
    } yield CachedSlackClient(cachedToken, client)
  }

}

object Main extends ZIOAppDefault {
  override val run = {
    for {
      fiber <- ZIO.foreachPar(1 to 10)(_ => loop).onInterrupt(ZIO.debug("HELP!")).fork
      _     <- Console.readLine
    } yield ()
  }.provide(CachedSlackClient.live, SlackClient.live)

  def loop: ZIO[CachedSlackClient, Throwable, Unit] =
    for {
      
      _     <- CachedSlackClient.sendMessage(s"I love not having a token")
      sleep <- Random.nextIntBetween(1, 3)
      _     <- ZIO.sleep(sleep.seconds)
      _     <- loop
    } yield ()

}

object DeadlineExample {

  trait ZDeadline {
    val underlying: scala.concurrent.duration.Deadline

    def timeLeft: UIO[FiniteDuration] =
      ZIO.succeed(underlying.timeLeft)
  }
//   case class Deadline private (time: FiniteDuration) extends Ordered[Deadline] {
//   /**
//    * Return a deadline advanced (i.e., moved into the future) by the given duration.
//    */
//   def +(other: FiniteDuration): Deadline = copy(time = time + other)
//   /**
//    * Return a deadline moved backwards (i.e., towards the past) by the given duration.
//    */
//   def -(other: FiniteDuration): Deadline = copy(time = time - other)
//   /**
//    * Calculate time difference between this and the other deadline, where the result is directed (i.e., may be negative).
//    */
//   def -(other: Deadline): FiniteDuration = time - other.time
//   /**
//    * Calculate time difference between this duration and now; the result is negative if the deadline has passed.
//    *
//    * '''''Note that on some systems this operation is costly because it entails a system call.'''''
//    * Check `System.nanoTime` for your platform.
//    */
//   def timeLeft: FiniteDuration = this - Deadline.now
//   /**
//    * Determine whether the deadline still lies in the future at the point where this method is called.
//    *
//    * '''''Note that on some systems this operation is costly because it entails a system call.'''''
//    * Check `System.nanoTime` for your platform.
//    */
//   def hasTimeLeft(): Boolean = !isOverdue()
//   /**
//    * Determine whether the deadline lies in the past at the point where this method is called.
//    *
//    * '''''Note that on some systems this operation is costly because it entails a system call.'''''
//    * Check `System.nanoTime` for your platform.
//    */
//   def isOverdue(): Boolean = (time.toNanos - System.nanoTime()) < 0
//   /**
//    * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
//    */
//   def compare(other: Deadline): Int = time compare other.time
// }
}
