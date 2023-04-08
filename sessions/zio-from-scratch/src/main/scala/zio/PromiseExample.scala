package zio

object PromiseExample extends App {

  val promiseExample =
    for {
      promise <- Promise.make[Nothing, Int]
      fiber   <- promise.await.flatMap(n => ZIO.succeed(println(s"consumer got $n"))).fork
      _       <- ZIO.succeed(Thread.sleep(1000))
      _       <- promise.succeed(42)
      _       <- fiber.join
    } yield ()

  promiseExample.unsafeRunSync()
}