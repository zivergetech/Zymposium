package api

import zio.json._

import java.util.UUID

final case class UserId(value: UUID) extends AnyVal

object UserId {
  implicit val codec: JsonCodec[UserId] = DeriveJsonCodec.gen
}

final case class User(userId: UserId, email: String)

object User {
  implicit val codec: JsonCodec[User] = DeriveJsonCodec.gen[User]
}
