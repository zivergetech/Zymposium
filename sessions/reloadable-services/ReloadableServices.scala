import zio._
import zio.macros._

trait Counter {
  def increment: UIO[Unit]
  def get: UIO[Int]
}
@scala.annotation.experimental
object Counter {

  val increment: ZIO[Counter, Nothing, Unit] =
    ZIO.serviceWithZIO(_.increment)

  val get: ZIO[Counter, Nothing, Int] =
    ZIO.serviceWithZIO(_.get)

  val live: ZLayer[Any, Nothing, Counter] =
    ZLayer {
      for {
        id     <- Ref.make(0)
        ref    <- Ref.make(0)
        service = CounterLive(id, ref)
        _      <- service.acquire
        _      <- ZIO.addFinalizer(service.release)
      } yield service
    }

  val reloadable: ZLayer[ServiceReloader, ServiceReloader.Error, Counter] =
    live.reloadable

  final case class CounterLive(id: Ref[Int], ref: Ref[Int]) extends Counter {
    def acquire: UIO[Unit] =
      Random.nextInt.flatMap(n => id.set(n) *> ZIO.debug(s"Acquired counter $n"))
    def increment: UIO[Unit] =
      ref.update(_ + 1)
    def get: UIO[Int] =
      ref.get
    def release: UIO[Unit] =
      id.get.flatMap(n => ZIO.debug(s"Released counter $n"))
  }

}

@scala.annotation.experimental
object ReloadableServices extends ZIOAppDefault {

  val app1 =
    for {
      _ <- Counter.increment
      _ <- Counter.increment
      _ <- Counter.increment
      n <- Counter.get
      _ <- Console.printLine(s"Counter value: $n").orDie
      _ <- ZIO.sleep(10.seconds)
      _ <- Counter.increment
      _ <- Counter.increment
      _ <- Counter.increment
      n <- Counter.get
      _ <- Console.printLine(s"Counter value: $n").orDie
    } yield ()

  trait FooService {
    def foo: UIO[Unit]
  }

  object FooService {
    val live: ZLayer[Any, Nothing, FooService] =
      ZLayer.succeed(new FooService {
        def foo: UIO[Unit] =
          Console.printLine("Foo").orDie
      })

    val reloadable =
      live.reloadable

    val foo: ZIO[FooService, Nothing, Unit] =
      ZIO.serviceWithZIO(_.foo)
  }

  val run =
    (FooService.foo *> (app1.zipPar(ServiceReloader.reload[Counter].delay(5.seconds)))).provide(
      Counter.reloadable,
      FooService.reloadable,
      ServiceReloader.live
    )

// Acquired counter 1995183774
// Counter value: 3
// Counter value: 6
// Released counter 1995183774

// Acquired counter 1513251939
// Counter value: 3
// Released counter 1513251939
// Acquired counter -1171319272
// Counter value: 3
// Released counter -1171319272
}
