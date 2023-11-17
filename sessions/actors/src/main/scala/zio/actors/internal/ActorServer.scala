package zio.actors.internal

import zio._
import zio.http._

object ActorServer {

  val send = Endpoints.send.implement(handler(ActorSystemService.send))
  
  val ask = Endpoints.ask.implement(handler(ActorSystemService.ask))

  val routes =
    Routes(send, ask)

  val app: HttpApp[ActorSystemService] =
    routes.toHttpApp
}