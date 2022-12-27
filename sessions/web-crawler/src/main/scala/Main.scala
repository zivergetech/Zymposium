import zio._

object Main extends ZIOAppDefault {

  val seeds =
    Set(URL.make("https://www.nytimes.com").get)

  val shouldCrawl = (_: URL) => true

  val processResult =
    (url: URL, _: String) => Console.printLine(url.toString).orDie

  val program =
    for {
      fiber <- WebCrawler.run(seeds, shouldCrawl, processResult).fork
      _     <- Console.readLine("Press Enter to exit!")
      _     <- fiber.interrupt
    } yield ()

  val run =
    program.provide(
      WebCrawler.live,
      Web.live
    )
}
