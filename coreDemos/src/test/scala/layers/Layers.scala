package layers

import zio.Console.printLine
import zio.test.ExecutionEvent.RuntimeFailure
import zio.test.{ExecutionEvent, ExecutionEventSink, SuiteId, TestFailure}
import zio._
import zio.logging.LogFormat.{fiberId, label, level, line, quoted, timestamp}
import zio.logging._

case class ExpensiveService(calls: Ref[Int]) {
  def call() =
    for {
      currentValue <- calls.updateAndGet(_+1)
//      _ <- printLine("Current mega value: " + currentValue)
//      _ <- ExecutionEventSink.process(RuntimeFailure(id= SuiteId(42), List.empty, TestFailure.fail("count time"), List.empty))
    } yield ()
  def finalResult = calls.get
}

object ExpensiveService {
  def call() =
    ZIO.serviceWithZIO[ExpensiveService](_.call())
  def finalResult() =
    ZIO.serviceWithZIO[ExpensiveService](_.finalResult)
}

case class AverageService(calls: Ref[Int]) {
  def call() = calls.update(_+1)
}

object AverageService {
  def call() =
    ZIO.serviceWithZIO[AverageService](_.call())
}

case class CheapService(calls: Ref[Int]) {
  def call() = calls.update(_+1)
}

object CheapService {
  def call() =
    ZIO.serviceWithZIO[CheapService](_.call())
}

object Layers {
  val average: ZLayer[Any, Nothing, AverageService] = ZLayer.scoped {
    for {
      counter <- Ref.make(0)
      service <-
        ZIO.acquireRelease(
          printLine("Constructing expensive layer").orDie *> ZIO.succeed(AverageService(counter))
        )(_ => printLine("Breaking down expensive layer").orDie)
    } yield service
  }

  def average(name: String): ZLayer[Any, Nothing, AverageService] = ZLayer.scoped {
    for {
      counter <- Ref.make(0)
      service <-
        ZIO.acquireRelease(
          ZIO.logWarning(s"Constructing average layer $name").provide(coloredLogger) *> ZIO.succeed(AverageService(counter))
        )(service => for {
          finalCount <- service.calls.get
          _ <- ZIO.logWarning(s"Releasing average layer $name after $finalCount calls").provide(coloredLogger)
        } yield ()
        )
    } yield service
  }

  def cheap(name: String): ZLayer[Any, Nothing, CheapService] = ZLayer.scoped {
    (for {
      counter <- Ref.make(0)
      service <-
        ZIO.acquireRelease(
           ZIO.log(s"Constructing cheap layer $name") *>  ZIO.succeed(CheapService(counter))
        )(service => for {
          finalCount <- service.calls.get
          _ <- ZIO.log(s"Releasing cheap layer $name after $finalCount calls!").provide(coloredLogger)
        } yield ()
        )
    } yield service).provideSome[Scope](coloredLogger)
  }

  val expensive: ZLayer[Any, Nothing, ExpensiveService] = ZLayer.scoped {
    (for {
      counter <- Ref.make(0)
      service <-
        ZIO.acquireRelease(
          ZIO.logError(s"Constructing expensive layer") *> ZIO.sleep(3.seconds).withClock(Clock.ClockLive) *> ZIO.succeed(ExpensiveService(counter))
        )(service => for {
          finalCount <- service.calls.get
          _ <- ZIO.logError(s"Releasing expensive layer after $finalCount calls")
        } yield ()
        )
    } yield service).provideSome[Scope](coloredLogger)
  }

  val colored: LogFormat =
//      label("thread", fiberId).color(LogColor.WHITE) |-|
      label("message", quoted(line)).highlight

  lazy val coloredLogger = {
    Runtime.removeDefaultLoggers >>> console(colored)
  }
}
