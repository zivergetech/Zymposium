package zio.actors.internal

import zio.http._
import zio.http.endpoint._

object Endpoints {

  val localId = long("id").transform(LocalId(_))(_.value)

  val send =
    Endpoint(Method.POST / "actors" / localId / "send").in[ActorRequest].out[Unit]

  val ask =
    Endpoint(Method.POST / "actors" / localId / "ask").in[ActorRequest].out[ActorResponse]
}
