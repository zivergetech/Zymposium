package zio.actors

import zio._

trait ActorSystem {
  def make[Environment, State, Message[+Response], Response](s: State)(
    f: (State, Message[Response]) => ZIO[Environment, Nothing, (State, Response)]
  ): ZIO[Environment, Nothing, ActorRef[Message]]
}

object ActorSystem {
  val make: ZIO[Scope, Nothing, ActorSystem] =
    for {
      scope   <- ZIO.scope
      counter <- Ref.make(0L)
      actors  <- Ref.make[Map[Long, Actor[_]]](Map.empty)
    } yield new ActorSystem {
      def make[Environment, State, Message[+Response], Response](s: State)(
        f: (State, Message[Response]) => ZIO[Environment, Nothing, (State, Response)]
      ): ZIO[Environment, Nothing, ActorRef[Message]] =
        for {
          actor <- scope.extend(Actor.make(s)(f))
          id    <- counter.getAndUpdate(_ + 1)
          _     <- actors.update(_ + (id -> actor))
        } yield new ActorRef[Message] {
          def send(message: Message[Any]): ZIO[Any, Nothing, Unit] =
            actor.send(message)
          def ask[Response](message: Message[Response]): ZIO[Any, Nothing, Response] =
            actor.ask(message)
        }
    }
}
