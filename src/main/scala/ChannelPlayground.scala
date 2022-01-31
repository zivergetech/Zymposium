import zio._
import zio.stream._
import java.io.FileOutputStream

object ChannelPlayground extends ZIOAppDefault {

  // Hello again. 

  // val run =
  //   Console.printLine("Hello, Zymposium!")

  // trait ZChannel[-Env, -InErr, -InElem, -InDone, +OutErr, +OutElem, +OutDone]

  // accept zero or more InElem values
  // eventually accept exactly one either InErr or InDone value

  // emit zero or more OutElem values
  // eventually if it terminates at all terminate with either OutErr or OutDone value

  // Streams, sinks, and pipelines are all specialized types of channels

  // type ZStream[-R, +E, +A] = ZChannel[R, Any, Any, Any, E, Chunk[A], Any]
  // type ZSink[R, In, E, L, Z] = ZChannel[R, Nothing, Chunk[In], Any, E, Chunk[L], Z]

  def mapStream[R, E, A, B](stream: ZStream[R, E, A])(f: A => B): ZStream[R, E, B] = {

    lazy val mapper: ZChannel[Any, E, Chunk[A], Any, E, Chunk[B], Any] =
      ZChannel.readWith(
        (in: Chunk[A]) => ZChannel.write(in.map(f)) *> mapper,
        (err: E) => ZChannel.fail(err),
        (done: Any) => ZChannel.succeed(done)
      )

    new ZStream(stream.channel >>> mapper)
  }

  // ZStream[R, E, A] = ZManaged[R, E, ZIO[R, E, Chunk[A]]]

  def grouped[R, E, A](stream: ZStream[R, E, A])(n: Int): ZStream[R, E, Chunk[A]] = {

    def grouper(leftovers: Chunk[A]): ZChannel[Any, E, Chunk[A], Any, E, Chunk[Chunk[A]], Any] =
      ZChannel.readWith(
        (in: Chunk[A]) => {
          val updatedChunk = leftovers ++ in
          val grouped: Chunk[Chunk[A]] = Chunk.fromIterator(updatedChunk.grouped(n))
          val last = grouped.lastOption
          last match {
            case None => grouper(Chunk.empty)
            case Some(chunk) =>
              if (chunk.length == n) {
                ZChannel.writeAll(grouped) *> grouper(Chunk.empty)
              } else {
                ZChannel.writeAll(grouped.dropRight(1)) *> grouper(chunk)
              }
          }
        },
        (err: E) => ZChannel.write(Chunk.single(leftovers)) *> ZChannel.fail(err),
        (done: Any) => ZChannel.write(Chunk.single(leftovers)) *> ZChannel.succeed(done)
      )


    new ZStream(stream.channel >>> grouper(Chunk.empty))
  }

  import java.io.IOException

  import java.io.{BufferedWriter, FileWriter}

  def fileWriter(path: String): ZChannel[Any, Nothing, Chunk[Any], Any, IOException, Nothing, Any] = {
    ZChannel.succeed(new BufferedWriter(new FileWriter(path))).flatMap[Any, Nothing, Chunk[Any], Any, IOException, Nothing, Any] { fileWriter =>

      lazy val writer: ZChannel[Any, Nothing, Chunk[Any], Any, IOException, Nothing, Any] =
        ZChannel.readWith[Any, Nothing, Chunk[Any], Any, IOException, Nothing, Any](
          in => {
            println(in)
            in.foreach { a =>
              fileWriter.write(a.toString)
              fileWriter.write("\n")
            }
            writer
          },
          err => {
            fileWriter.close()
            ZChannel.fail(err)
          },
          done => {
            fileWriter.close()
            ZChannel.succeed(done)
          }
        )

      writer
    }
  }

  def fileWriterSink(path: String) =
    new ZSink(fileWriter(path))

  val stream1 = ZStream(1, 2, 3)
  val stream2 = grouped(stream1)(2)

  val channel =
    stream2.channel >>> fileWriter("cool.txt")

  val run =
    // stream2.run(fileWriterSink(path))
    channel.runDrain
}
