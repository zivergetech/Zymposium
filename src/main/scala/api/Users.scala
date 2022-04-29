package api

import zio.{Random, Ref, ZIO, ZLayer}
import zio.macros.accessible

trait Users {
  def create(email: String): ZIO[Any, Throwable, User]
  def get(id: UserId): ZIO[Any, Throwable, Option[User]]
  def all: ZIO[Any, Throwable, List[User]]
}

object Users {
  def create(email: String): ZIO[Users, Throwable, User] =
    ZIO.serviceWithZIO(_.create(email))

  def get(id: UserId): ZIO[Users, Throwable, Option[User]] =
    ZIO.serviceWithZIO(_.get(id))

  def all: ZIO[Users, Throwable, List[User]] =
    ZIO.serviceWithZIO(_.all)

  val live =
    ZLayer(Ref.make(Map.empty[UserId, User])) >>> ZLayer.fromFunction(UsersLive.apply _)
}

final case class UsersLive(ref: Ref[Map[UserId, User]]) extends Users {
  override def create(email: String): ZIO[Any, Throwable, User] =
    for {
      id  <- Random.nextUUID.map(UserId(_))
      user = User(id, email)
      _   <- ref.update(_ + (id -> user))
    } yield user

  override def get(id: UserId): ZIO[Any, Throwable, Option[User]] =
    ref.get.map(_.get(id))

  override def all: ZIO[Any, Throwable, List[User]] =
    ref.get.map(_.values.toList)
}
