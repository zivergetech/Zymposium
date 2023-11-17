package zio.actors.internal

import zio._
import zio.schema._

final case class ActorRequest(value: Chunk[Byte])

object ActorRequest {

  implicit val schema: Schema[ActorRequest] =
    DeriveSchema.gen[ActorRequest]
}