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
