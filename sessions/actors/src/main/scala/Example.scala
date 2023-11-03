import zio._
import zio.actors._

object Example {

  sealed trait TemperatureMessage[+Response]

  object TemperatureMessage {
    final case class SetTemperature(value: Double) extends TemperatureMessage[Unit]
    case object GetTemperature                     extends TemperatureMessage[Double]
  }

  def makeTemperatureActor(actorSystem: ActorSystem): ZIO[Any, Nothing, ActorRef[TemperatureMessage]] =
    actorSystem.make(0.0) {
      case (state, TemperatureMessage.SetTemperature(value)) =>
        ZIO.succeed(value -> ())
      case (state, TemperatureMessage.GetTemperature) =>
        ZIO.succeed(state -> state)
    }
}

object Actors extends ZIOAppDefault {
  import Example._

  val run =
    for {
      actorSystem <- ActorSystem.make
      actor       <- makeTemperatureActor(actorSystem)
      _           <- actor ! TemperatureMessage.SetTemperature(42.0)
      temp        <- actor ? TemperatureMessage.GetTemperature
      _           <- Console.printLine(s"Temperature is $temp")
    } yield ()
}
