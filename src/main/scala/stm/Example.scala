package stm

import zio._

object Example extends ZIOAppDefault {

  val run =
    for {
      ref   <- TRef.make(0).commit
      update = ref.get.flatMap(n => ref.set(n + 1)).commit
      _     <- ZIO.foreachPar(1 to 100)(_ => update)
      n     <- ref.get.commit
      _     <- Console.printLine(n)
    } yield ()
}

object Example2 extends ZIOAppDefault {

  val run =
    for {
      ref     <- TRef.make(0).commit
      workflow = ref.get.flatMap(n => if (n == 0) STM.retry else STM.succeed(1)).commit
      fiber   <- workflow.flatMap(n => Console.printLine(n)).fork
      _       <- ZIO.sleep(1.second)
      _       <- ref.set(1).commit
      _       <- fiber.await
    } yield ()
}
