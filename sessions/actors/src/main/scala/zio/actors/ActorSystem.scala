package zio.actors

import zio._
import zio.actors.internal._
import zio.http._
import zio.schema._

trait ActorSystem {
  def make[Environment, State, Message[+Response], Response](s: State)(
    f: (State, Message[Response]) => ZIO[Environment, Nothing, (State, Response)]
  )(implicit requestSchema: Schema[Message[Response]], responseSchema: Schema[Response]): ZIO[Environment, Nothing, ActorRef[Message]]
  private[actors] def unsafeMake[Message[+Response], Response](actorId: ActorId): ZIO[Any, Nothing, ActorRef[Message]]
}

object ActorSystem {

  val make: ZIO[Scope & Config, Nothing, ActorSystem] =
    for {
      scope       <- ZIO.scope
      config      <- ZIO.service[Config]
      serverConfig = Server.Config.default.binding(config.host, config.port)
      location     = Location(config.host, config.port)
      counter     <- Ref.make(0L)
      actors      <- Ref.make[Map[LocalId, (Actor[_], Schema[_], Schema[_])]](Map.empty)
      _ <- Server
             .serve(ActorServer.app)
             .provide(ActorSystemService.live, Server.live, ZLayer.succeed(serverConfig), ZLayer.succeed(actors))
             .forkScoped
    } yield new ActorSystem {
      def make[Environment, State, Message[+Response], Response](s: State)(
        f: (State, Message[Response]) => ZIO[Environment, Nothing, (State, Response)]
      )(implicit requestSchema: Schema[Message[Response]], responseSchema: Schema[Response]): ZIO[Environment, Nothing, ActorRef[Message]] =
        for {
          actor <- scope.extend(Actor.make(s)(f))
          id    <- counter.getAndUpdate(_ + 1)
          _     <- actors.update(_ + (LocalId(id) -> (actor, requestSchema, responseSchema)))
        } yield new ActorRef[Message] {
          def send(message: Message[Any]): ZIO[Any, Nothing, Unit] =
            actor.send(message)
          def ask[Response](message: Message[Response]): ZIO[Any, Nothing, Response] =
            actor.ask(message)
        }
      def unsafeMake[Message[+Response], Response](actorId: ActorId): ZIO[Any, Nothing, ActorRef[Message]] =
        ZIO.succeed {
          new ActorRef[Message] {
            def send(message: Message[Any]): ZIO[Any, Nothing, Unit] =
              if (actorId.location == location)
                actors.get.flatMap { map =>
                  map.get(actorId.localId) match {
                    case Some((actor, _, _)) => actor.asInstanceOf[Actor[Message]].send(message)
                    case None        => ZIO.dieMessage(s"Actor with id $actorId does not exist")
                  }
                }
              else
                ???
            def ask[Response](message: Message[Response]): ZIO[Any, Nothing, Response] =
              if (actorId.location == location)
                actors.get.flatMap { map =>
                  map.get(actorId.localId) match {
                    case Some((actor, _, _)) => actor.asInstanceOf[Actor[Message]].ask(message)
                    case None        => ZIO.dieMessage(s"Actor with id $actorId does not exist")
                  }
                }
              else
                ???
          }
        }
    }

  def make(host: String, port: Int): ZIO[Scope, Nothing, ActorSystem] =
    make.provideSome[Scope](ZLayer.succeed(Config(host, port)))

  final case class Config(host: String, port: Int)
}
