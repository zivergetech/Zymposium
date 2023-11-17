package zio.actors.internal

import zio.http._
import zio.http.endpoint._

object Endpoints {

  val send =
    Endpoint(Method.POST / "actors" / long("id") / "send").in[ActorRequest].out[Unit]

  val ask =
    Endpoint(Method.POST / "actors" / long("id") / "ask").in[ActorRequest].out[ActorResponse]
}