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

    // ZIO[R, E, A]
    // ZStream[R, E, A]

    // In --> Processing --> Out
    // Fan Out // broadcast operations

    // In --> Processing1 --> Out
    //    \-> Processing2 -/

    // In == Stream
    // () => A
    // ZIO[R, E, A]
    // ZStream[R, E, A]

    // A => Z // one A value
    // A => ZIO[R, E, B] == continuation
    // zio.flatMap(f)

    // A => Z // many A values
    // ZSink

    // final case class ZStream[-R, +E, +A](
    //     process: ZManaged[R, E, ZIO[R, Option[E], A]]
    // ) {

    // Singleton Chunkifactory
    def run[R1 <: R, E1 >: E, O](sink: ZSink[R1, E1, A, O]): ZIO[R1, E1, O] =
      process.zip(sink.run).use { case (pull, push) =>
        def loop: ZIO[R1, E1, O] =
          pull.foldZIO(
            {
              case Some(e) => ZIO.fail(e)
              case None =>
                push(Chunk.empty).flatMap {
                  case Some(o) => ZIO.succeed(o)
                  case None =>
                    ZIO.dieMessage(
                      "Sink violated contract by returning None after being pushed empty Chunk"
                    )
                }
            },
            a =>
              push(Chunk.single(a)).flatMap {
                case Some(o) => ZIO.succeed(o)
                case None    => loop
              }
          )
        loop
      }

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

    import java.io.File
    import java.io.FileWriter
    import java.io.BufferedWriter

    // Stream[R, A, String]
    def runToFile(
        name: String
    )(implicit ev: A <:< String): ZIO[R, E, Unit] =
      process.use { pull =>
        ZIO.succeed(new File(name)).flatMap { file =>
          ZIO.succeed(new BufferedWriter(new FileWriter(file))).flatMap {
            writer =>
              lazy val loop: ZIO[R, E, Unit] =
                pull.foldZIO(
                  {
                    case Some(e) => ZIO.fail(e) // stream failed with an error
                    case None    => ZIO.succeed(()) // stream end
                  },
                  a =>
                    ZIO.succeed(
                      writer.write(a)
                    ) *> loop // stream emitted an element
                )
              loop.ensuring(ZIO.effectTotal(writer.close()))
          }
        }
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

  // final case class ZStream[-R, +E, +A](
  //     process: ZManaged[R, E, ZIO[R, Option[E], A]]
  // ) {

  type ??? = Nothing

  // CHUNK WORLD is a world of chunks
  // "Chunky" Monster

  // Protocol

  // Empty chunk of input means no more values
  // Succeed with Some(O) means "I'm done with a summary value O"
  // Succeed with None means "feed me more input"
  // Fail with E means "I'm done with an error E"
  //
  // runCollect: ZSink[R, E, A, Chunk[A]]
  // "Leftovers"

  // trait StreamSupervisor[R, E, A, B] {
  //   def run(stream: ZStream[R, E, A], sink: ZSink[R, E, A, B]): ZIO[R, E, B]
  // }

  final case class ZSink[-R, +E, -I, +O](
      run: ZManaged[R, E, Chunk[I] => ZIO[R, E, Option[O]]]
  ) { self =>

    def zipWithPar[R1 <: R, E1 >: E, I1 <: I, O2, O3](
        that: ZSink[R1, E1, I1, O2]
    )(f: (O, O2) => O3): ZSink[R1, E1, I1, O3] = {

      sealed trait State[+O, +O2]
      case object Running extends State[Nothing, Nothing]
      final case class LeftDone(o: O) extends State[O, Nothing]
      final case class RightDone(o: O2) extends State[Nothing, O2]

      ZSink {
        self.run
          .zipPar(that.run)
          .zipPar(Ref.make[State[O, O2]](Running).toManaged)
          .map { case (pushLeft, pushRight, ref) =>
            in =>
              ref.get.flatMap {
                case Running =>
                  pushLeft(in).zipPar(pushRight(in)).flatMap {
                    case (Some(o), Some(o2)) => ZIO.succeed(Some(f(o, o2)))
                    case (Some(o), None)     => ref.set(LeftDone(o)).as(None)
                    case (None, Some(o2))    => ref.set(RightDone(o2)).as(None)
                    case (None, None)        => ZIO.succeed(None)
                  }
                case LeftDone(o) =>
                  pushRight(in).map {
                    case Some(o2) => Some(f(o, o2))
                    case None     => None
                  }
                case RightDone(o2) =>
                  pushLeft(in).map {
                    case Some(o) => Some(f(o, o2))
                    case None    => None
                  }

              }
          }
      }
    }
  }

  object ZSink {

    def runCollect[A]: ZSink[Any, Nothing, A, Chunk[A]] =
      ZSink {
        Ref.make[Chunk[A]](Chunk.empty).toManaged.map { ref => in =>
          if (in.isEmpty) ref.get.asSome
          else ref.update(_ ++ in).as(None)
        }
      }

    import java.io.BufferedWriter
    import java.io.FileWriter

    def writer(file: String): ZManaged[Any, Throwable, BufferedWriter] =
      ZManaged
        .make(
          ZIO.effect(
            new BufferedWriter(new FileWriter(file))
          )
        )(reader => ZIO.effectTotal(reader.close()))

    def toFile(file: String): ZSink[Any, Throwable, String, Unit] =
      ZSink {
        writer(file).map { writer => in =>
          if (in.isEmpty) ZIO.succeed(Some(()))
          else
            ZIO
              .foreachDiscard(in)(s => ZIO.effectTotal(writer.write(s)))
              .as(None)
        }
      }
  }

  val myRealStream =
    ZStream.fromIterator(Iterator.fromList(List(1, 2, 3, 4, 6, 7)))

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
      .lines("./src/main/scala/cool.txt") // in
      .tap(a => ZIO.debug(s"WOW $a").delay(1.second)) // middle
      .runCollect // end

  val simpleStream =
    ZStream.fromIterator(Iterator.fromList(List(1, 2, 3, 4, 6, 7)))

  val simpleSink =
    ZSink.runCollect[String]

  val simpleSink2 =
    ZSink.toFile("./src/main/scala/output.txt")

  val notSoSimpleSink =
    simpleSink.zipWithPar(simpleSink2) { case (l, _) => l }

  lazy val simpleStreamProgram =
    simpleStream.map(_ * 3).map(_.toString).run(notSoSimpleSink).debug

  val run =
    simpleStreamProgram

  // fileStream
  //     .runToFile("./src/main/scala/cool2.txt") // out

  // myFakeStream.delay(1.second) //Console.printLine("Hello, from ZIO!")

}

object Channels {
  type ??? = Nothing

  object Old {
    final case class ZStream[-Env, +OutErr, +OutElem, +OutDone](
        pull: ZManaged[
          Env,
          OutErr,
          ZIO[Env, OutErr, Either[Chunk[OutElem], OutDone]]
        ]
    )
    final case class ZSink[-Env, +OutErr, -InElem, +OutDone](
        push: ZManaged[Env, OutErr, Chunk[InElem] => ZIO[Env, OutErr, Option[
          OutDone
        ]]]
    )
  }

  final case class ZChannel[
      -Env,
      -InErr,
      -InElem,
      -InDone,
      +OutErr,
      +OutElem,
      +OutDone
  ](
      run: Managed[InErr, IO[InErr, Either[InElem, InDone]]] => //
      ZManaged[Env, OutErr, ZIO[Env, OutErr, Either[OutElem, OutDone]]]
  ) { self =>

    // compose
    // f: A => B
    // g: B => C
    // f >>> g
    // h: A => C 
    def >>>[Env1 <: Env, OutErr2, OutElem2, OutDone2](
      that: ZChannel[Env1, OutErr, OutElem, OutDone, OutErr2, OutElem2, OutDone2]
    ): ZChannel[Env1, InErr, InElem, InDone, OutErr2, OutElem2, OutDone2] =
      ZChannel { upstream =>
        ZManaged.environment[Env].flatMap { environment =>
          that.run(self.run(upstream).map(_.provideEnvironment(environment)).provideEnvironment(environment))
        }
      }

    def mapElem[OutElem2](f: OutElem => OutElem2): ZChannel[
      Env,
      InErr,
      InElem,
      InDone,
      OutErr,
      OutElem2,
      OutDone
    ] =
      ZChannel { upstream =>
       run(upstream).map(_.map(_.left.map(f)))
      }

    // Managed[InErr, IO[InErr, Either[InElem, InDone]]] => //
    // ZManaged[Env, OutErr, ZIO[Env, OutErr, Either[OutElem, OutDone]]]
    def runDrain(implicit ev1: Any <:< InErr, ev2: Any <:< InElem, ev3: Any <:< InDone): ZIO[Env, OutErr, (Chunk[OutElem], OutDone)] =
      run(ZManaged.succeed(ZIO.succeed(Right(())))).use { pull =>
            def loop(acc: Chunk[OutElem]): ZIO[Env, OutErr, (Chunk[OutElem], OutDone)] = 
               pull.flatMap {
                 case Left(elem)  => loop(acc :+ elem)
                 case Right(done) => ZIO.succeed(acc -> done)
               }
            loop(Chunk.empty)
          }
         
        
  }

  object ZChannel {

    def fromIteratorChunk[A](
        iterator: => Iterator[A],
        chunkSize: Int = 1
    ): ZChannel[Any, Any, Any, Any, Nothing, Chunk[A], Any] =
      ZChannel { _ =>
        ZManaged.succeed(iterator.grouped(chunkSize)).map { iterator =>
          ZIO.succeed(iterator.hasNext).flatMap { b =>
            if (b) ZIO.succeed(Left(Chunk.fromIterable(iterator.next())))
            else ZIO.succeed(Right(()))
          }
        }
      }
  }

  final case class ZStream[-R, +E, +A](
      channel: ZChannel[R, Any, Any, Any, E, Chunk[A], Any]
  ) { self =>

    def map[B](f: A => B): ZStream[R, E, B] =
      ZStream(channel.mapElem(_.map(f)))

      //  found   : Channels.ZChannel[R1,E2,,Z]
      //   [error]  required: zio.ZIO[R1,E2,Z]

    def run[R1 <: R, E2, Z](sink: ZSink[R1, E, E2, A, Z]): ZIO[R1, E2, Z] =
      (self.channel >>> sink.channel).runDrain.map(_._2)
  }

  object ZStream {

    def fromIterator[A](
        iterator: => Iterator[A],
        chunkSize: Int = 1
    ): ZStream[Any, Nothing, A] =
      ZStream(ZChannel.fromIteratorChunk(iterator, chunkSize))
  }

      // -Env,
      // -InErr,
      // -InElem,
      // -InDone,
      // +OutErr,
      // +OutElem,
      // +OutDone
  final case class ZSink[-R, -EIn, +EOut, -I, +O](
    channel: ZChannel[R, EIn, Chunk[I], Any, EOut, Nothing, O]
  )

  object ZSink {
    
    // A -> Chunk[A]
    def runCollect[E, A]: ZSink[Any, E, E, A, Chunk[A]] =
      ZSink {
        ZChannel { upstream =>
          upstream.map { pull =>
            def loop(acc: Chunk[A]): IO[E, Either[Nothing, Chunk[A]]] =
              pull.flatMap {
                case Left(chunk) => loop(acc ++ chunk)
                case Right(_)    => ZIO.succeed(Right(acc))
              }
            loop(Chunk.empty)
          }
        }
      }
      
  }

  // Input Type = Requirement
  // Output Type = Expectation
  // Requirements have a minimum expectation, the more you know the more you expect of a type and the less you can accept (-)
  // Expectations get maximally vague, the more you return, the less you know but the more you can accept (+)

}

object Example extends ZIOAppDefault {
  import Channels._

  val stream = ZStream.fromIterator(Iterator(1, 2, 3, 4, 5)).map(_ * 2)

  val run =
    stream.run(ZSink.runCollect).debug

}
