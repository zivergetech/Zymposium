package zio.actors.internal

import zio._
import zio.http._

object ActorServer {

  val send =
    Endpoints.send.implement {
      Handler.fromFunctionZIO[(Long, ActorRequest)] { case (id: Long, actorRequest: ActorRequest) =>
        ZIO.dieMessage("Not implemented")
      }
    }
  
  val ask =
    Endpoints.ask.implement {
      Handler.fromFunctionZIO[(Long, ActorRequest)] { case (id: Long, actorRequest: ActorRequest) =>
        ZIO.dieMessage("Not implemented")
      }
    }
}