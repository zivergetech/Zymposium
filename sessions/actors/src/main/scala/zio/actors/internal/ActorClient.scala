package zio.actors.internal

import zio._
import zio.http._
import zio.http.endpoint._
import zio.schema._
import zio.schema.codec._

sealed trait ActorClient {
  def send[Message[+Response]](actorId: ActorId, message: Message[Any])(implicit requestSchema: Schema[Message[Any]]): ZIO[Any, Nothing, Unit]
  def ask[Message[+Response], Response](actorId: ActorId, message: Message[Response])(implicit requestSchema: Schema[Message[Response]], responseSchema: Schema[Response]): ZIO[Any, Nothing, Response]
}

object ActorClient {

  val live: ZLayer[Client, Nothing, ActorClient] =
    ZLayer {
      for {
        client <- ZIO.service[Client]
      } yield new ActorClient {
        def send[Message[+Response]](actorId: ActorId, message: Message[Any])(implicit requestSchema: Schema[Message[Any]]): ZIO[Any, Nothing, Unit] = {
          val endpointLocator = EndpointLocator.fromURL(actorId.url)
          val endpointExecutor = EndpointExecutor(client, endpointLocator, ZIO.unit)
          val requestBinaryCodec = JsonCodec.schemaBasedBinaryCodec(requestSchema)
          val actorRequest = ActorRequest(requestBinaryCodec.encode(message))
          retryOnConnectionRefused(ZIO.scoped(endpointExecutor(Endpoints.send(actorId.localId, actorRequest))))
        }
        def ask[Message[+Response], Response](actorId: ActorId, message: Message[Response])(implicit requestSchema: Schema[Message[Response]], responseSchema: Schema[Response]): ZIO[Any, Nothing, Response] = {
          val endpointLocator = EndpointLocator.fromURL(actorId.url)
          val endpointExecutor = EndpointExecutor(client, endpointLocator, ZIO.unit)
          val requestBinaryCodec = JsonCodec.schemaBasedBinaryCodec(requestSchema)
          val actorRequest = ActorRequest(requestBinaryCodec.encode(message))
          retryOnConnectionRefused(ZIO.scoped(endpointExecutor(Endpoints.ask(actorId.localId, actorRequest)))).flatMap { actorResponse =>
            val responseBinaryCodec = JsonCodec.schemaBasedBinaryCodec(responseSchema)
            responseBinaryCodec.decode(actorResponse.value) match {
              case Left(error)    => ZIO.dieMessage(error.toString)
              case Right(response) => ZIO.succeed(response)
            }
          }
        }
      }
    }

  private def retryOnConnectionRefused[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.catchAllCause { cause =>
      cause.dieOption match {
        case Some(throwable) if throwable.getMessage.contains("Connection refused") => retryOnConnectionRefused(zio)
        case _ => ZIO.refailCause(cause)
      }  
    }
}