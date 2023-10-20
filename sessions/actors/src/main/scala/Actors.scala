import zio._

trait Actor[-Message[+Response]] {
  def !(message: Message[Any]): ZIO[Any, Nothing, Unit]                    = send(message)
  def ?[Response](message: Message[Response]): ZIO[Any, Nothing, Response] = ask(message)
  def send(message: Message[Any]): ZIO[Any, Nothing, Unit]
  def ask[Response](message: Message[Response]): ZIO[Any, Nothing, Response]
}

object Example {

  sealed trait TemperatureMessage[+Response]

  object TemperatureMessage {
    final case class SetTemperature(value: Double) extends TemperatureMessage[Unit]
    case object GetTemperature                     extends TemperatureMessage[Double]
  }

  final case class TemperatureActor(inbox: Queue[(TemperatureMessage[Any], Promise[Nothing, _])])
      extends Actor[TemperatureMessage] {
    def send(message: TemperatureMessage[Any]): ZIO[Any, Nothing, Unit] =
      for {
        promise <- Promise.make[Nothing, Unit]
        _       <- inbox.offer((message, promise))
      } yield ()
    def ask[Response](message: TemperatureMessage[Response]): ZIO[Any, Nothing, Response] =
      for {
        promise  <- Promise.make[Nothing, Response]
        _        <- inbox.offer((message, promise))
        response <- promise.await
      } yield response
  }

  def makeTemperatureActor: ZIO[Scope, Nothing, Actor[TemperatureMessage]] =
    for {
      inbox <- Queue.unbounded[(TemperatureMessage[Any], Promise[Nothing, _])]
      state <- Ref.make(0.0)
      _ <- inbox.take.flatMap { case (message, promise) =>
             message match {
               case TemperatureMessage.SetTemperature(value) =>
                 state.set(value) *> promise.asInstanceOf[Promise[Nothing, Unit]].succeed(())
               case TemperatureMessage.GetTemperature =>
                 state.get.flatMap(a => promise.asInstanceOf[Promise[Nothing, Double]].succeed(a))
             }
           }.forever.forkScoped
    } yield TemperatureActor(inbox)
}

object Actors extends ZIOAppDefault {
  import Example._

  val run =
    for {
      actor <- makeTemperatureActor
      _     <- actor ! TemperatureMessage.SetTemperature(42.0)
      temp  <- actor ? TemperatureMessage.GetTemperature
      _     <- Console.printLine(s"Temperature is $temp")
    } yield ()
}
