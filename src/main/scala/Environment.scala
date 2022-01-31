import zio._

object Environment extends ZIOAppDefault {

  // - Building layers was hard!
  //   >>>
  //   ++

  // - Error messages were really bad!
  // type mismatch;
  //  found   : zio.ZLayer[zio.magic.Example.Spoon.Spoon with Any,Nothing,zio.magic.Example.Cake.Cake with zio.console.Console]
  //     (which expands to)  zio.ZLayer[zio.Has[zio.magic.Example.Spoon.Service] with Any,Nothing,zio.Has[zio.magic.Example.Cake.Service] with zio.Has[zio.console.Console.Service]]
  //  required: zio.ULayer[zio.magic.Example.Cake.Cake with zio.console.Console]
  //     (which expands to)  zio.ZLayer[Any,Nothing,zio.Has[zio.magic.Example.Cake.Service] with zio.Has[zio.console.Console.Service]]
  //       ((Flour.live) ++ (Spoon.live >>> Chocolate.live)) >>> Cake.live ++ Console.live

  // Has - what is a has and what is not a has
  //    Logging = ???
  //    type Logging = Has[Logging.Service]

  //    trait Logging
  //    Has[Logging]

  // Weird patterns to avoid Has in type signatures
  //
  // object logging {
  //   type Logging = Has[Logging.Service]
  //   trait Service
  // }

  // object ZLayer {}

  // Simplify layer combination
  // Provide simple and idiomatic way to define services
  // Eliminate Has
  // Provide simple and idiomatic way to construct layers

  // Best Practices

  // 1. Define your service as a normal trait
  // 2. Define accessors for your service
  // 3  Create a live implementation of your service as a case class with any
  //    dependencies as constructor parameters
  // 4. Turn your live implementation into a layer
  // 5. Wire your layers together to run your application

  // package logging
  
  trait WorldCrusher {
    def crush: UIO[Unit]
  }

  object WorldCrusher {
    def crush = ZIO.serviceWithZIO[WorldCrusher](_.crush)

    val live = (WorldCrusherLive.apply _).toLayer[WorldCrusher]
  }

  case class GemOfPower(powerLevel: Int)

  case class WorldCrusherLive(console: Console, logging: Logging, gem: GemOfPower) extends WorldCrusher {
    def crush = logging.logLine(s"CRUSH! CRUSH! CRUSH! ${gem}")
  }


  trait Logging {
    // 1. GENERALLY USE EFFECTS
    // 2. MAKE SURE THE ENVIRONMENT IS Any
    def logLine(line: String): ZIO[Any, Nothing, Unit]
    // def port: UIO[Int]
  }

  object Logging {

    def logLine(line: String): ZIO[Logging, Nothing, Unit] =
      ZIO.serviceWithZIO(_.logLine(line))

  }

  // package logging-console
  final case class ConsoleLogging(console: Console, clock: Clock)
      extends Logging {

    def logLine(line: String): ZIO[Any, Nothing, Unit] =
      for {
        now <- clock.currentDateTime
        _ <- console.printLine(s"$now: $line").orDie
      } yield ()
  }

  // Has[Console with Clock]

  // ZEnvironment(
  //   Console.type -> Console.live$121212,
  //   Clock.type -> Clock.live$121212
  // )

  // // ZIO 1.0
  // R => Either[Cause[E], A]

  // // ZIO 2.0
  // ZEnvironment[R] => Either[Cause[E], A]

  val environment: ZEnvironment[Int & String] =
    ZEnvironment.empty.add(42).add("Hello")

  val myInt = environment.get[Int]
  val myString = environment.get[String]

  println(myInt)
  println(myString)

  // val myDouble = environment.get[Double]

  object ConsoleLogging {

    val registerLoggingService: ZIO[Any, Nothing, Unit] =
      ZIO.unit

    val layer: URLayer[Console with Clock, ConsoleLogging] =
      (ConsoleLogging.apply _).toLayer

    val manualSolidLayerPatternForAllCases
        : ZLayer[Console with Clock, Nothing, Logging] =
      ZLayer {
        for {
          _ <- ZIO.debug("BLENDING BEGINETH!").toManaged_
          _ <- registerLoggingService.toManaged_
          console <- ZManaged.service[Console]
          clock <- ZManaged.service[Clock]
          _ <- ZManaged.finalizer(ZIO.debug("BLENDING COMPLETE!"))
        } yield ConsoleLogging(console, clock)
      }
  }

  val myZIO: ZIO[Logging & WorldCrusher, Nothing, Unit] =
    for {
      _ <- Logging.logLine("Hello")
      _ <- ZIO.serviceWithZIO[Logging](_.logLine("Hello from a more verbose implementation"))
      _ <- ZIO.environment[Logging & WorldCrusher].flatMap(environment => environment.get[Logging].logLine("Hello from an even more verbose implementation"))
      _ <- WorldCrusher.crush
    } yield ()

  val fullLayer: ZLayer[Any, Nothing, Logging] =
    ZLayer.make[Logging](
      ConsoleLogging.manualSolidLayerPatternForAllCases,
      Console.live,
      Clock.live
    )

  val gemLayer = ZLayer.succeed(GemOfPower(9001))
  
  val effect = myZIO.provide(
      ConsoleLogging.layer, 
      Console.live, 
      Clock.live,
      WorldCrusher.live,
      gemLayer
    )

  val run =
    myZIO.provideCustom(fullLayer, WorldCrusher.live, gemLayer)
}
