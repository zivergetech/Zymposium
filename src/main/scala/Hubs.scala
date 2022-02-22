import zio._
import zio.stream._

object Hubs extends ZIOAppDefault {

  def producer(queue: Queue[Int]): ZIO[Clock, Nothing, Unit] =
    ZIO.foreachDiscard(0 to 5) { i =>
      queue.offer(i) *> ZIO.sleep(100.milliseconds)
    }

  def consumer(
      label: String
  )(queue: Queue[Int]): ZIO[Console, Nothing, Nothing] =
    queue.take.flatMap { i =>
      Console.printLine(s"$label got $i").!
    }.forever

  def hubProducer(hub: ZHub[Any, Any, Nothing, Nothing, Int, Any]): ZIO[Clock, Nothing, Unit] =
    ZIO.foreachDiscard(0 to 5) { i =>
      hub.publish(i) *> ZIO.sleep(100.milliseconds)
    }

  def hubConsumer(
      label: String
  )(hub: ZHub[Any, Any, Nothing, Nothing, Nothing, String])(currentSubscribers: Ref[Int], start: Promise[Nothing, Unit]): ZIO[Console, Nothing, Nothing] =
    hub.subscribe.use { dequeue =>
      currentSubscribers.updateAndGet(_ + 1).flatMap { n =>
        if (n == 4) start.succeed(())
        else ZIO.unit  
      } *>
      dequeue.take.flatMap { i =>
        Console.printLine(s"$label got $i").!
      }.forever
    }

  // Naive hub
  // index   0 1 2 3 4
  // Queue1 | |2|3|4|5| |
  // Queue2 | | |3|4|5| |

  // Hub
  // index   0 1 2 3 4
  // Array  | |2|3|4|5| |
  // consumer1 index == 1
  // consumer2 index == 2
  // minIndex == 1

  final case class NaiveHub[A](queues: Chunk[Queue[A]]) {
    def offer(a: A): ZIO[Any, Nothing, Unit] =
      ZIO.foreachDiscard(queues)(_.offer(a))

    def take(n: Int): ZIO[Any, Nothing, A] =
      queues(n).take
  }

  object NaiveHub {
    def make[A](n: Int): ZIO[Any, Nothing, NaiveHub[A]] =
      ZIO
        .foreach(Chunk.fromIterable(0 until n))(i => Queue.bounded[A](16))
        .map(qs => NaiveHub(qs))
  }

  // Queues -> Work Distribution
  // Hubs   -> Work Broadcasting

  // Queue and Hub types
  // Unbounded - unlimited capacity, publishing never backpressures
  // Bounded - limited capacity, publishing backpressures if full
  // Sliding - limited capacity, drop oldest element if full
  // Dropping - limited capacity, drops newest element

  val run =
    streamExample3

  lazy val hubExample =
    for {
      hub <- Hub.bounded[String](16)
      mapperHub = hub
        .contramap[Int](_.toString + "!!!!")
        .mapZIO(s => ZIO.debug(s"someone took $s from the hub").as(s))
      ref <- Ref.make(0)
      promise <- Promise.make[Nothing, Unit]
      _ <- hubConsumer("logging framework")(mapperHub)(ref, promise).delay(200.millis).fork
      _ <- hubConsumer("database writer")(mapperHub)(ref, promise).delay(200.millis).fork
      _ <- hubConsumer("data eater")(mapperHub)(ref, promise).fork
      _ <- hubConsumer("info destroyer")(mapperHub)(ref, promise).fork
      // _ <- promise.await
      _ <- hubProducer(mapperHub)
      _ <- ZIO.sleep(1.second)
    } yield ()

  lazy val queueExample =
    for {
      queue <- Queue.bounded[Int](16)
      producerFiber <- producer(queue).fork
      _ <- consumer("logging framework")(queue).fork
      _ <- consumer("database writer")(queue).fork
      _ <- producerFiber.join
    } yield ()

  lazy val sourceStream: ZStream[Clock, Nothing, Int] =
    ZStream(1, 2, 3, 4, 5).tap(_ => ZIO.sleep(100.milliseconds))

  lazy val streamExample =
    sourceStream.broadcast(4, 16).use { streams =>
      val consumedStreams =
        Chunk(
          streams(0) via consume[Int]("logging framework"),
          streams(1) via consume[Int]("database writer"),
          streams(2) via consume[Int]("data eater"),
          streams(3) via consume[Int]("info destroyer")
        )

      ZStream.mergeAllUnbounded(16)(consumedStreams: _*).runDrain
    }

  lazy val streamExampleDynamic =
    sourceStream.broadcastDynamic(16).use { subscribe =>
      val consumedStreams =
        Chunk(
          subscribe via consume[Int]("logging framework"),
          subscribe via consume[Int]("database writer"),
          subscribe via consume[Int]("data eater"),
          subscribe via consume[Int]("info destroyer")
        )

      ZStream.mergeAllUnbounded(16)(consumedStreams: _*).runDrain
    }

    // streams give us the ability to "pull" from them
    // try to "pull" from primary stream

  // Take
  // Represents different potential results from taking from a stream
  // Chunk of values
  // Error
  // End of stream signal

  lazy val streamExample3 =
    for {
      hub    <- Hub.bounded[Int](16)
      stream = ZStream.fromHub(hub)
      _ <- hub.publish(10).forever.fork
      _ <- stream.take(10).foreach { a => Console.printLine(a.toString)}
    } yield ()

  // Non streaming              Hub#publish, Hub#subscribe
  // Stream to stream           ZStream#broadcast, ZStream#broadcastDynamic
  // Stream to non-stream       ZStream#runIntoHub
  // non-stream to stream       ZStream.fromHub

  def consume[A](label: String): ZPipeline[Console, Nothing, A, A] =
    ZPipeline.mapZIO(a => Console.printLine(s"$label got $a").orDie.as(a))
}
