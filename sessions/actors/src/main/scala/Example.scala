import zio._
import zio.actors._
import zio.schema._

// Plan
// Goal: we can send a message to an ActorRef from an ActorSystem in one process
// and have that message get executed by an Actor that is running on an ActorSystem
// in another process
// And do this in a way where we can convince ourselves that if those processes were
// on different machines, it would still work
// 1. Introduce concept of an ActorId
//    - Location - host and port of the ActorSystem where this actor is physically located
//    - Id       - unique identifier within a physical instantiation of an ActorSystem
//    (Location + Id) uniquely identify an actor // localhost:8080/1, localhost:8080/2, localhost:8081/1
// 2. Each time we start an ActorSystem in a process we also start a server that corresponds to that ActorSystem
// 3. When we send a message to an ActorRef associated with an ActorSystem, we do the following:
//    A. We check whether the location of the actor is on our machine
//    B. If so, run it as before
//    C. If not, forward the request to the server that corresponds to that ActorSystem and either wait
//       or not depending on if it is a ask or tell
// 4. Hypothesis is use ZIO HTTP to implement the server
// 5. Will need to deal with serialization and deserialization of messages, will probably use ZIO Schema

object Example {

  sealed trait TemperatureMessage[+Response]

  object TemperatureMessage {
    final case class SetTemperature(value: Double) extends TemperatureMessage[TemperatureResponse.SetTemperature.type]
    case object GetTemperature                     extends TemperatureMessage[TemperatureResponse.GetTemperature]

    implicit val schema: Schema[TemperatureMessage[TemperatureResponse]] =
      DeriveSchema.gen[TemperatureMessage[TemperatureResponse]]
  }

  sealed trait TemperatureResponse

  object TemperatureResponse {
    case object SetTemperature               extends TemperatureResponse
    case class GetTemperature(value: Double) extends TemperatureResponse

    implicit val schema: Schema[TemperatureResponse] =
      DeriveSchema.gen[TemperatureResponse]
  }

  def makeTemperatureActor(actorSystem: ActorSystem): ZIO[Any, Nothing, ActorRef[TemperatureMessage]] =
    actorSystem.make[Any, Double, TemperatureMessage, TemperatureResponse](0.0) {
      case (state, TemperatureMessage.SetTemperature(value)) =>
        ZIO.succeed(value -> TemperatureResponse.SetTemperature)
      case (state, TemperatureMessage.GetTemperature) =>
        ZIO.succeed(state -> TemperatureResponse.GetTemperature(state))
    }
}

object Actors extends ZIOAppDefault {
  import Example._

  val run =
    for {
      actorSystem <- ActorSystem.make("localhost", 8080)
      actor       <- makeTemperatureActor(actorSystem)
      _           <- actor ! TemperatureMessage.SetTemperature(42.0)
      temp        <- actor ? TemperatureMessage.GetTemperature
      _           <- Console.printLine(s"Temperature is $temp")
    } yield ()
}
