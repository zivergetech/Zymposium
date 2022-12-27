import zio._

trait WebCrawler {
  def run(
    seeds: Set[URL],
    shouldCrawl: URL => Boolean,
    processResult: (URL, String) => UIO[Any]
  ): ZIO[Any, Nothing, Unit]
}

object WebCrawler {

  def run(
    seeds: Set[URL],
    shouldCrawl: URL => Boolean,
    processResult: (URL, String) => UIO[Unit]
  ): ZIO[WebCrawler, Nothing, Unit] =
    ZIO.serviceWithZIO(_.run(seeds, shouldCrawl, processResult))

  val live: ZLayer[Web, Nothing, WebCrawler] =
    ZLayer.fromFunction(WebCrawlerLive(_))

  final case class WebCrawlerLive(web: Web) extends WebCrawler {

    val defaultParallelism: Int = 8

    def run(
      seeds: Set[URL],
      shouldCrawl: URL => Boolean,
      processResult: (URL, String) => UIO[Any]
    ): UIO[Unit] = {

      def worker(
        queue: Queue[URL],
        ref: Ref[CrawlState]
      ): IO[Throwable, Unit] =
        for {
          url        <- queue.take
          html       <- web.getURL(url)
          urls        = extractURLs(url, html)
          urlsToCrawl = urls.filter(shouldCrawl)
          newUrls    <- modifyCrawlState(ref, urlsToCrawl)
          _          <- processResult(url, html)
          _          <- queue.offerAll(newUrls)
        } yield ()

      def modifyCrawlState(ref: Ref[CrawlState], urls: Set[URL]): UIO[Set[URL]] =
        ref.modify { crawlState =>
          (urls -- crawlState.visited, crawlState.visitAll(urls))
        }

      for {
        queue <- Queue.unbounded[URL]
        ref   <- Ref.make(CrawlState.empty)
        _     <- queue.offerAll(seeds)
        _ <- ZIO.collectAllParDiscard {
               Chunk.fill(defaultParallelism)(worker(queue, ref).ignore.forever)
             }
      } yield ()
    }

    // take the initial seeds and grab those web pages
    // Extract all the links from the web pages
    // Repeat with all of those
    // Need some kind of state to keep track of where we have visited
    // Scaleable or long running

    // Ref that represents what we have already visited
    // queue that represents "work that needs to be done"
    // create a fixed number of fibers that each pull a value from a queue, process
    // and do that forever

    // fiber lifetime
    // make sure we don't interrupt these fibers ourselves before the user is done
    // when the user is done we interrupt these fibers

  }

  def extractURLs(root: URL, html: String): Set[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util.Try {
      val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toSet

      for {
        m   <- matches
        url <- URL.make(m) ++ root.relative(m)
      } yield url
    }
      .getOrElse(Set.empty)
  }

}
