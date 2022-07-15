package layers

import zio.test.{Spec, TestAspect, ZIOSpec, ZIOSpecDefault, assertCompletes, test}
import Tools._
import zio.{ZIOAppDefault, ZLayer}

// TODO Adam/Kit - Think these are helpful for more compact examples,
//    or just obfuscating?
object Tools {
  def testCheapService(name: String): Spec[CheapService, Nothing] =
    test(name)(
      for {
        _ <- CheapService.call
      } yield assertCompletes
    )

  def testAllServices(name: String) =
    test(name)(
      for {
        _ <- CheapService.call
        _ <- AverageService.call
        _ <- ExpensiveService.call
      } yield assertCompletes
    )
}

//object HelloTest extends  ZIOSpecDefault {
//  def spec =
//    test("hi")(assertCompletes)
//}
//
//object CheapServiceForEverySpecExplicit extends ZIOSpecDefault {
//  def spec =
//    suite("uses a cheap layer")(
//      testCheapService("A")
//        .provide(Layers.cheap),
//      testCheapService("B")
//        .provide(Layers.cheap),
//    )
//}
//
//object CheapServiceForEverySpec extends ZIOSpecDefault {
//  def spec =
//    suite("uses a cheap layer")(
//      testCheapService("A"),
//      testCheapService("B"),
//    ).provide(Layers.cheap)
//}
//
//object AverageServiceRebuiltSpec extends ZIOSpecDefault {
//
//  def spec =
//    suite("uses a shared layer")(
//      test("A")(
//        for {
//          _ <- AverageService.call
//        } yield assertCompletes
//      ),
//      test("B")(
//        for {
//          _ <- AverageService.call
//        } yield assertCompletes
//      ),
//    ).provide(Layers.average)
//}
//
//object AverageServiceSharedSpec extends ZIOSpecDefault {
//
//  def spec =
//    suite("uses a shared layer")(
//      test("A")(
//        for {
//          _ <- AverageService.call
//        } yield assertCompletes
//      ),
//      test("B")(
//        for {
//          _ <- AverageService.call
//        } yield assertCompletes
//      ),
//    ).provideShared(Layers.average)
//}
//
//object MixedServiceSpec extends ZIOSpecDefault {
//
//  def spec =
//    suite("uses a shared layer")(
//      test("A")(
//        for {
//          _ <- CheapService.call
//          _ <- AverageService.call
//        } yield assertCompletes
//      ),
//      test("B")(
//        for {
//          _ <- CheapService.call
//          _ <- AverageService.call
//        } yield assertCompletes
//      ),
//    ).provideShared(Layers.average, Layers.cheap)
//}
//
//object MixedServiceSplitLayersSpec extends ZIOSpecDefault {
//
//  def spec =
//    suite("uses a shared layer")(
//      test("A")(
//        for {
//          _ <- CheapService.call
//          _ <- AverageService.call
//        } yield assertCompletes
//      ),
//      test("B")(
//        for {
//          _ <- CheapService.call
//          _ <- AverageService.call
//        } yield assertCompletes
//      ),
//    )
//      .provideSome[AverageService](Layers.cheap)
//      .provideShared(Layers.average)
//}
//
//// TODO Duplicate when this is in its final form, and delete variant below
//object NestedSpecsWithMixedSharingNonbootstrapSpecA extends ZIOSpecDefault {
//  def spec =
//    suite("Nested Spec")(
//      suite("First Suite")(
//        testAllServices("A"),
//        testAllServices("B")
//      ),
//
//      suite("Second Suite")(
//        testAllServices("A"),
//        testAllServices("B")
//      )
//    ).provideSome[AverageService with ExpensiveService](Layers.cheap)
//      .provideShared(Layers.expensive, Layers.average)
//}
//
//object NestedSpecsWithMixedSharingNonbootstrapSpecB extends ZIOSpecDefault {
//  def spec =
//    suite("Nested Spec")(
//      suite("First Suite")(
//        testAllServices("A"),
//        testAllServices("B")
//      ),
//      suite("Second Suite")(
//        testAllServices("A"),
//        testAllServices("B")
//      )
//    ).provideSome[AverageService with ExpensiveService](Layers.cheap)
//      .provideShared(Layers.expensive, Layers.average)
//}
//
//
//// TODO Duplicate when this is in its final form, and delete variant below
//object BootstrapSpecsWithMixedSharingSpecA extends ZIOSpec[ExpensiveService] {
//  val bootstrap: ZLayer[Any, Nothing, ExpensiveService] = Layers.expensive
//
//  def spec =
//    suite("Nested Spec")(
//      suite("First Suite")(
//        testAllServices("A"),
//        testAllServices("B")
//      ),
//      suite("Second Suite")(
//        testAllServices("A"),
//        testAllServices("B")
//      )
//    )
//      .provideSome[AverageService with ExpensiveService](Layers.cheap)
//      .provideSomeShared[ExpensiveService](Layers.average) @@ DemoTools.reportExpensiveLayer
//
//}
//
//object BootstrapSpecsWithMixedSharingSpecB extends ZIOSpec[ExpensiveService] {
//  val bootstrap = Layers.expensive
//
//  def spec =
//    suite("Nested Spec")(
//      suite("First Suite")(
//        testAllServices("A"),
//        testAllServices("B")
//      ),
//      suite("Second Suite")(
//        testAllServices("A"),
//        testAllServices("B")
//      )
//    )
//      .provideSome[AverageService with ExpensiveService](Layers.cheap)
//      .provideSomeShared[ExpensiveService](Layers.average)
//}
//
