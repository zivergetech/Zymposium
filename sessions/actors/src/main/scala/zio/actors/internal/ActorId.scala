package zio.actors.internal

import zio.http._

final case class ActorId(location: Location, localId: LocalId) {
  def url: URL = url"http://${location.host}:${location.port}"
}