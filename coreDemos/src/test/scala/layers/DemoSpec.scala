package layers

import zio._
import zio.test._

// "SHARING IS CARING" - Bill Frasure

// Create two different layers that implement two different implementations of ExpensiveService

// DemoSpecA ==> val bootstrap = expensiveServiceLayer1
// DemoSpecB ==> val boostrap = expensiveServiceLayer2

import DemoTools._

object DemoSpecA extends ZIOSpec[ExpensiveService] {
  val bootstrap = Layers.expensive
  def spec = {
    (suite("suite A")(
      test("A")(
        for {
          _ <- CheapService.call
          _ <- AverageService.call
          _ <- ExpensiveService.call
        } yield assertCompletes
      ),
      test("B")(
        for {
          _ <- CheapService.call
          _ <- AverageService.call
          _ <- ExpensiveService.call
        } yield assertCompletes
      ),
    ).provideSome[AverageService with ExpensiveService](CheapService.live)
    )
      .provideSomeShared(Layers.average)
  }
}

object DemoSpecB extends ZIOSpec[ExpensiveService] {
  val bootstrap = Layers.expensive
  def spec = {
    (suite("suite B")(
      test("A")(
        for {
          _ <- CheapService.call
          _ <- AverageService.call
          _ <- ExpensiveService.call
        } yield assertCompletes
      ),
      test("B")(
        for {
          _ <- CheapService.call
          _ <- AverageService.call
          _ <- ExpensiveService.call
        } yield assertCompletes
      ),
    ).provideSome[AverageService with ExpensiveService](CheapService.live) @@ DemoTools.reportExpensiveLayer
      )
      .provideSomeShared(Layers.average)
  }
}


case class CheapService(calls: Ref[Int]) {
  val call: UIO[Unit] =
    calls.update(_ + 1)
}

import DemoTools._

object CheapService {
  val call: URIO[CheapService, Unit] =
    ZIO.serviceWithZIO[CheapService](_.call)


  val live: ZLayer[Any, Nothing, CheapService] =
    ZLayer.scoped {
      for {
        _       <- log("Creating cheap service")
        counter <- Ref.make(0)
        service  = CheapService(counter)
        _       <- ZIO.addFinalizer(
          for {
            finalCount <- counter.get
            _ <- log(s"Releasing cheap service after $finalCount calls")
          } yield ()
        )
      } yield service
  }
}