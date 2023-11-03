package zio.actors

import zio._

trait Actor[-Message[+Response]] {
  def !(message: Message[Any]): ZIO[Any, Nothing, Unit]                    = send(message)
  def ?[Response](message: Message[Response]): ZIO[Any, Nothing, Response] = ask(message)
  def send(message: Message[Any]): ZIO[Any, Nothing, Unit]
  def ask[Response](message: Message[Response]): ZIO[Any, Nothing, Response]
}

object Actor {

  private[actors] def make[Environment, State, Message[+Response], Response](s: State)(
    f: (State, Message[Response]) => ZIO[Environment, Nothing, (State, Response)]
  ): ZIO[Environment & Scope, Nothing, Actor[Message]] =
    for {
      queue <- Queue.unbounded[(Message[Any], Promise[Nothing, _])].withFinalizer(_.shutdown)
      ref   <- Ref.make(s)
      _ <- queue.take.flatMap { case (message, promise) =>
             ref.get.flatMap { state =>
               f(state, message.asInstanceOf[Message[Response]]).flatMap { case (state, response) =>
                 ref.set(state) *> promise.asInstanceOf[Promise[Nothing, Response]].succeed(response)
               }
             }
           }.forever.forkDaemon
    } yield new Actor[Message] {
      def send(message: Message[Any]): ZIO[Any, Nothing, Unit] =
        for {
          promise <- Promise.make[Nothing, Unit]
          _       <- queue.offer((message, promise))
        } yield ()
      def ask[Response](message: Message[Response]): ZIO[Any, Nothing, Response] =
        for {
          promise  <- Promise.make[Nothing, Response]
          _        <- queue.offer((message, promise))
          response <- promise.await
        } yield response
    }
}
