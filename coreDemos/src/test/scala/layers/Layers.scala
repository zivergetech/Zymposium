package layers

import layers.Layers.coloredLogger
import zio._
import zio.logging.LogFormat.{fiberId, label, level, line, quoted, timestamp}
import zio.logging._
import zio.test.TestAspect

import DemoTools._


case class AverageService(calls: Ref[Int]) {
  val call = calls.update(_+1)
}

object AverageService {
  val call =
    ZIO.serviceWithZIO[AverageService](_.call)
}

case class ExpensiveService(calls: Ref[Int]) {
  val call =
      calls.update(_+1)

  val logFinalResult = {
    for {
      result <- calls.get
      _ <- logError("Releasing expensive service after " + result + " calls!").provide(Layers.coloredLogger)
    } yield ()
  }
}

object ExpensiveService {
  val call =
    ZIO.serviceWithZIO[ExpensiveService](_.call)
  val logFinalResult =
    ZIO.serviceWithZIO[ExpensiveService](_.logFinalResult)
}

object Layers {
  val average: ZLayer[Any, Nothing, AverageService] = ZLayer.scoped {
    for {
      counter <- Ref.make(0)
      service <-
        ZIO.acquireRelease(
          logWarning("Constructing average service").provide(coloredLogger) *> ZIO.sleep(2.seconds).withClock(Clock.ClockLive) *> ZIO.succeed(AverageService(counter))
        )(service => for {
          finalCount <- service.calls.get
          _ <- logWarning(s"Releasing average service after $finalCount calls!").provide(coloredLogger)
        } yield ()
        )
    } yield service
  }

  val expensive: ZLayer[Any, Nothing, ExpensiveService] = ZLayer.scoped {
    (for {
      counter <- Ref.make(0)
      service <-
        ZIO.acquireRelease(
          logError(s"Constructing expensive service").provide(coloredLogger) *> ZIO.sleep(4.seconds).withClock(Clock.ClockLive) *> ZIO.succeed(ExpensiveService(counter))
        )(service => for {
          finalCount <- service.calls.get
          _ <- logError(s"Releasing expensive service after $finalCount calls").provide(coloredLogger)
        } yield ()
        )
    } yield service)
  }

  val colored: LogFormat =
//      label("thread", fiberId).color(LogColor.WHITE) |-|
      label("message", quoted(line)).highlight


  lazy val coloredLogger = {
    Runtime.removeDefaultLoggers >>> console(colored)
  }
}
