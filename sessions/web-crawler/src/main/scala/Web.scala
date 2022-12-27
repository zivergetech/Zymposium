import zio._

trait Web {
  def getURL(url: URL): ZIO[Any, Throwable, String]
}

object Web {

  val live: ZLayer[Any, Nothing, Web] =
    ZLayer.succeed(WebLive())

  final case class WebLive() extends Web {
    def getURL(url: URL): ZIO[Any, Throwable, String] =
      ZIO.attemptBlockingIO {
        scala.io.Source.fromURL(url.toString).mkString
      }
  }

}
