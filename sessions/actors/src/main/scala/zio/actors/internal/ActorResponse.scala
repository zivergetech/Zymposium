package zio.actors.internal

import zio._
import zio.schema._

final case class ActorResponse(value: Chunk[Byte])

object ActorResponse {

  implicit val schema: Schema[ActorResponse] =
    DeriveSchema.gen[ActorResponse]
}