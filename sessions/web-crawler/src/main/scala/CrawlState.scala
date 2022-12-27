final case class CrawlState(visited: Set[URL]) {
  def visit(url: URL): CrawlState   = copy(visited = visited + url)
  def visitAll(urls: Iterable[URL]) = copy(visited = visited ++ urls)
  def hasVisited(url: URL): Boolean = visited.contains(url)
}

object CrawlState {
  val empty: CrawlState =
    CrawlState(Set.empty)
}
