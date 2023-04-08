package zio

sealed trait FiberMessage

object FiberMessage {

  case object Start                                        extends FiberMessage
  final case class AddObserver(cb: Exit[Any, Any] => Unit) extends FiberMessage
  case object Interrupt                                    extends FiberMessage
  final case class Resume(zio: ZIO[Any, Any])              extends FiberMessage
}