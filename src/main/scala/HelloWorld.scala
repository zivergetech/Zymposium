import zio._

object Streaming extends ZIOAppDefault {

  /**   - Streams are incremental
    *   - Streams unfold over time
    *   - ZIO yieldss ZERO or ONE result
    *   - ZStream yieldss ZERO or ONE or TWO or THREE or ... results
    */
  val myInt: ZIO[Any, Nothing, Int] =
    ZIO.succeed(42)

  val myFakeStream =
    Console
      .printLine(42)
      .delay(100.milliseconds)
      .repeat(Schedule.recurs(10))
      .as(42)
      .debug("FINAL OUTPUT")

  val myFakeStream2: ZIO[Any, Nothing, Chunk[Int]] =
    ZIO.succeed(Chunk(42, 56, 78))

  // Iterator
  trait Iterator[+A] {
    def hasNext: Boolean
    def next(): A

    def foreach(f: A => Unit): Unit =
      while (hasNext) f(next())
  }

  object Iterator {

    def fromList[A](list: List[A]): Iterator[A] =
      new Iterator[A] {
        private var current = list
        def hasNext: Boolean = current.nonEmpty
        def next(): A = {
          val head = current.head
          current = current.tail
          head
        }
      }

    // def fromFile(file: File): Iterator[String] =
    //   new Iterator[String] {
    //     private val source = scala.io.Source.fromFile(file)
    //     def hasNext: Boolean = source.hasNext
    //     def next(): String = source.next()
    //   }

    val x = fromList(List(1, 2, 3))
  }

  // 1                    2               3
  // Int                | String        | None
  // Success(Some(Int)) | Success(None) | Error(String)
  // val zio1: ZIO[Any, String, Option[Int]] = ???
  // 1              2            3
  // Int          | String      | None
  // Success(Int) | Error(None) | Error(Some(String))
  // val zio2: ZIO[Any, Option[String], Int] = zio1.some
  // val zio3: ZIO[Any, Nothing, Either[String, Option[Int]]] = ???

  // Effectual iterator
  // Cardinality
  final case class ZStream[-R, +E, +A](
      process: ZManaged[R, E, ZIO[R, Option[E], A]]
  ) {
    import ZStream.Pull

    def tap[R1 <: R](f: A => URIO[R1, Any]): ZStream[R1, E, A] =
      ZStream(process.map(_.tap(f)))

    def map[B](f: A => B): ZStream[R, E, B] =
      ZStream(process.map(_.map(f)))

    // example: ZStream.fromIterator(
    //            Iterator.fromList(List(1, 2, 3))
    //          ).take(2).runCollect == Chuck(List(1, 2))
    def take(n: Int): ZStream[R, E, A] =
      ZStream(
        Ref.make(0).toManaged.zip(process).map { case (ref, pull) =>
          ref.getAndUpdate(_ + 1).flatMap { i =>
            if (i >= n) Pull.end
            else pull
          }
        }
      )

    def runCollect: ZIO[R, E, Chunk[A]] =
      process.use { pull =>
        val builder = ChunkBuilder.make[A]
        lazy val loop: ZIO[R, E, Chunk[A]] =
          pull.foldZIO(
            {
              case Some(e) => ZIO.fail(e) // stream failed with an error
              case None    => ZIO.succeed(builder.result()) // stream end
            },
            a => ZIO.succeed(builder += a) *> loop // stream emitted an element
          )
        loop
      }
  }

  object ZStream {

    object Pull {
      def emit[A](a: A) = ZIO.succeed(a)
      val end = ZIO.fail(None)
    }

    def fromIterator[A](iterator: => Iterator[A]): ZStream[Any, Nothing, A] =
      ZStream(
        ZManaged.succeed(iterator).map { iterator =>
          ZIO.succeed(iterator.hasNext).flatMap { b =>
            if (b) Pull.emit(iterator.next())
            else Pull.end
          }
        }
      )

    import java.io.BufferedReader
    import java.io.FileReader

    def lines[A](file: String): ZStream[Any, Throwable, String] =
      ZStream(
        ZManaged
          .make(
            ZIO.effect(
              new BufferedReader(new FileReader(file))
            )
          )(reader => ZIO.effectTotal(reader.close()))
          .map { reader =>
            ZIO.attempt(reader.ready()).asSomeError.flatMap { b =>
              if (b) ZIO.attempt(reader.readLine()).asSomeError
              else ZIO.fail(None)
            }
          }
      )
  }

  val myRealStream =
    ZStream.fromIterator(Iterator.fromList(List(1, 2, 3, 4, 6, 7)))

  val run =
    fileStream.runCollect
  // for {
  //   chunk <- myRealStream
  //     .tap(a => ZIO.debug(s"WOW $a"))
  //     .map(_ * 2)
  //     .tap(a => ZIO.debug(s"BIGGER WOW $a"))
  //     .take(5)
  //     .tap(_ => ZIO.sleep(1.second))
  //     .runCollect
  //   _ <- ZIO.debug(chunk)
  //   _ <- second
  // } yield ()

  lazy val second = UIO {
    List(1, 2, 3, 4, 6, 7)
      .map(a => { println(s"LIST WOW $a"); a })
      .map(_ * 2)
      .map(a => { println(s"LIST BIGGER WOW $a"); a })
      .take(3)
  }

  lazy val fileStream =
    ZStream
      .lines("./src/main/scala/cool.txt")
      .tap(a => ZIO.debug(s"WOW $a").delay(1.second))

  // myFakeStream.delay(1.second) //Console.printLine("Hello, from ZIO!")

}
