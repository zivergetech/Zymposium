package caching

import java.time.Instant
import zio._

final case class AuthToken(value: Long, expiration: Instant) {
  def isExpired(now: Instant): Boolean = expiration.isBefore(now)
}

case class SlackClient(ref: Ref[Set[AuthToken]]) {
  def refreshToken: UIO[AuthToken] =
    for {
      _    <- Console.print("Getting Refresh Token").!
      _    <- Console.print(".").delay(100.millis).repeatN(10).!
      _    <- Console.print("\n").!
      now  <- Clock.instant
      long <- Random.nextLong
      token = AuthToken(long, now.plusSeconds(4))
      _    <- ref.update(_.filterNot(_.isExpired(now)) + token)
    } yield token

  def postMessage(message: String, token: AuthToken): Task[Unit] =
    for {
      now      <- Clock.instant
      tokens   <- ref.get
      isInvalid = !tokens(token) || token.isExpired(now)
      _        <- ZIO.fail(new Error("Invalid auth token")).when(isInvalid)
      _        <- ZIO.debug(s"#zio: ${scala.Console.CYAN}$message")
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
}

object Main extends ZIOAppDefault {
  override val run = {
    for {
      token <- SlackClient.refreshToken
      _     <- ZIO.sleep(3.seconds)
      _     <- SlackClient.postMessage("Hello, world!", token)
    } yield ()
  }.provide(SlackClient.live)
}
