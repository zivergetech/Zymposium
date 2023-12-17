package zio.actors.internal

import zio._
import zio.actors._
import zio.schema._
import zio.schema.codec._

trait ActorSystemService {
  def send(id: LocalId, actorRequest: ActorRequest): ZIO[Any, Nothing, Unit]
  def ask(id: LocalId, actorRequest: ActorRequest): ZIO[Any, Nothing, ActorResponse]
}

object ActorSystemService {

  val live: ZLayer[ActorsState, Nothing, ActorSystemService] =
    ZLayer {
      for {
        actors <- ZIO.service[ActorsState]
      } yield new ActorSystemService {
        def send(id: LocalId, actorRequest: ActorRequest): ZIO[Any, Nothing, Unit] =
          ZIO.debug(s"Server received send $id $actorRequest") *> actors.get.flatMap { map =>
            map.get(id) match {
              case Some((actor, requestSchema, _)) =>
                val requestBinaryCodec = JsonCodec.schemaBasedBinaryCodec(requestSchema)
                requestBinaryCodec.decode(actorRequest.value) match {
                  case Left(error)    => ZIO.dieMessage(error.toString)
                  case Right(message) => actor.asInstanceOf[Actor[AnyMessage]].send(message)
                }
              case None => ZIO.dieMessage(s"Actor with id $id does not exist")
            }
          }.debug(s"Server sent send $id $actorRequest")
        def ask(id: LocalId, actorRequest: ActorRequest): ZIO[Any, Nothing, ActorResponse] =
          ZIO.debug(s"Server received ask $id $actorRequest") *> actors.get.flatMap { map =>
            map.get(id) match {
              case Some((actor, requestSchema, responseSchema)) =>
                val requestBinaryCodec = JsonCodec.schemaBasedBinaryCodec(requestSchema)
                requestBinaryCodec.decode(actorRequest.value) match {
                  case Left(error)    => ZIO.dieMessage(error.toString)
                  case Right(message) => actor.asInstanceOf[Actor[AnyMessage]].ask(message).map { (response: Any) =>
                    val responseBinaryCodec = JsonCodec.schemaBasedBinaryCodec(responseSchema.asInstanceOf[Schema[Any]])
                    val value = responseBinaryCodec.encode(response)
                    ActorResponse(value)
                  }
                }
              case None => ZIO.dieMessage(s"Actor with id $id does not exist")
            }
          }.debug(s"Server sent ask $id $actorRequest")
      }
    }

  def send(id: LocalId, actorRequest: ActorRequest): ZIO[ActorSystemService, Nothing, Unit] =
    ZIO.serviceWithZIO(_.send(id, actorRequest))

  def ask(id: LocalId, actorRequest: ActorRequest): ZIO[ActorSystemService, Nothing, ActorResponse] =
    ZIO.serviceWithZIO(_.ask(id, actorRequest))
}
