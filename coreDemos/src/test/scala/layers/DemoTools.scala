package layers

import layers.Layers.coloredLogger
import zio.test.TestAspect
import zio.{Trace, UIO, ZIO}

object DemoTools {

  val reportExpensiveLayer = TestAspect.afterAll(
    ExpensiveService.logFinalResult
  )

  def logError(message: => String)(implicit trace: Trace): UIO[Unit] =
    ZIO.logError(message).provide(Layers.coloredLogger)

  def log(message: => String)(implicit trace: Trace): UIO[Unit] =
    ZIO.log(message).provide(coloredLogger)

  def logWarning(message: => String)(implicit trace: Trace): UIO[Unit] =
    ZIO.logWarning(message).provide(coloredLogger)
}
