package zio

object RefExample extends App {

  // Fiber A reads 0
  // Fiber B reads 0
  // Fiber A writes 1 (0 + 1)
  // Fiber B writes 1 (0 + 1)
  // WRONG ANSWER!!!

  val refExample: ZIO[Nothing, Unit] =
    for {
      ref <- Ref.make(1000) // pieces of work that need to be done
      _ <- ref.modify { oldValue =>
             val newValue = oldValue - 1
             val workflow = if (newValue == 0) ZIO.succeed(println("All done!")) else ZIO.unit
             (workflow, newValue)
           }.flatten.fork.repeatN(999)
      _     <- ZIO.succeed(Thread.sleep(1000))
      value <- ref.get
      _     <- ZIO.succeed(println(value))
    } yield ()

    // Fiber A --> Do some work --> 2 --> Still 2 left to do!
    // Fiber B --> Do some work --> 1 --> Still 1 left to do!
    // Fiber C --> Do some work --> 0 --> All done!
    // All done is not going to be run until every has finished doing their work

  refExample.unsafeRunSync()
}
