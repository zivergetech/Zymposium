package layers

import zio.ZIO
import zio.test.{TestAspect, ZIOSpec, ZIOSpecDefault, assertCompletes, test}

object Tools {
  def simpleTest(name: String) =
    test(name)(
      for {
        _ <- CheapService.call()
      } yield assertCompletes
    )

  def complexTest(name: String) =

    test(name)(
      for {
        _ <- CheapService.call()
        _ <- AverageService.call()
        _ <- ExpensiveService.call()
      } yield assertCompletes
    )
}

object ServiceForEverySpec extends ZIOSpecDefault {
  def spec =
    suite("uses a shared layer")(
      Tools.simpleTest("A"),
      Tools.simpleTest("B"),
    ).provide(Layers.cheap("X") ++ Layers.coloredLogger)

}

object LogSpec extends ZIOSpecDefault {
  def spec =
    suite("uses a shared layer")(
      test("log something")(
        ZIO.logError("should be red").map(_ => assertCompletes)
      )
    ).provide(Layers.coloredLogger)

}

object IntraSpecSharingSpec extends ZIOSpecDefault {

  def spec =
    suite("uses a shared layer")(
      test("A")(
        for {
          _ <- AverageService.call()
        } yield assertCompletes
      ),
      test("B")(
        for {
          _ <- AverageService.call()
        } yield assertCompletes
      ),
    ).provideShared(Layers.average)

}
object ProvidedToEachTestSpec extends ZIOSpecDefault {

  def spec =
    suite("uses a shared layer")(
      test("A")(
        for {
          _ <- AverageService.call()
        } yield assertCompletes
      ).provide(Layers.average),
      test("B")(
        for {
          _ <- AverageService.call()
        } yield assertCompletes
      ).provide(Layers.average),
    )

}

object ProvidedUnsharedToSuiteSpec extends ZIOSpecDefault {

  def spec =
    suite("uses a shared layer")(
      test("A")(
        for {
          _ <- ZIO.unit
        } yield assertCompletes
      ),
      test("B")(
        for {
          _ <- ZIO.service[AverageService]
          _ <- ZIO.unit
        } yield assertCompletes
      ),
    ).provide(Layers.average)

}

object NestedSpecsWithMixedSharingSpec extends ZIOSpec[ExpensiveService] {
  val bootstrap = Layers.expensive

  def spec =
    suite("X: uses a shared layer")(
      suite("First Suite")(
        Tools.complexTest("1A"),
        Tools.complexTest("1B")
      ).provideSomeLayerShared[AverageService with ExpensiveService](Layers.cheap("A")),

      suite("Second Suite")(
        Tools.complexTest("2A"),
        Tools.complexTest("2B")
      ).provideSomeShared[AverageService with ExpensiveService](Layers.cheap("X: B"))

    ).provideSomeShared[ExpensiveService](Layers.average("X: SpecLevel")) @@ TestAspect.afterAll(
      ExpensiveService.finalResult().debug("Final result")
    )

}



object NestedSpecsWithMixedSharing2Spec extends ZIOSpec[ExpensiveService] {
  val bootstrap = Layers.expensive

  def spec =
    suite("Y: uses a shared layer")(
      suite("Y: first suite")(
        Tools.complexTest("1A"),
        Tools.complexTest("1B"),
      ).provideSomeShared[AverageService with ExpensiveService](Layers.cheap("A")),
      suite("Y: second suite")(
        Tools.complexTest("2A"),
        Tools.complexTest("2B")
      ).provideSomeShared[AverageService with ExpensiveService](Layers.cheap("Y: B"))
    ).provideSomeShared[ExpensiveService](Layers.average("Y: SpecLevel"))

}
