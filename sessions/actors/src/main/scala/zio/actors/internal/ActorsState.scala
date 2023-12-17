package zio.actors.internal

import zio._
import zio.actors._
import zio.schema._

sealed trait ActorsState {
 def get: ZIO[Any, Nothing, Map[LocalId, (Actor[_], Schema[_], Schema[_])]]
 def update(f: Map[LocalId, (Actor[_], Schema[_], Schema[_])] => Map[LocalId, (Actor[_], Schema[_], Schema[_])]): ZIO[Any, Nothing, Unit]
}

object ActorsState {

  val make: ZIO[Any, Nothing, ActorsState] =
    for {
      ref <- Ref.make[Map[LocalId, (Actor[_], Schema[_], Schema[_])]](Map.empty)
    } yield new ActorsState {
      def get: ZIO[Any, Nothing, Map[LocalId, (Actor[_], Schema[_], Schema[_])]] =
        ref.get
      def update(f: Map[LocalId, (Actor[_], Schema[_], Schema[_])] => Map[LocalId, (Actor[_], Schema[_], Schema[_])]): ZIO[Any, Nothing, Unit] =
        ref.update(f)
    }
}