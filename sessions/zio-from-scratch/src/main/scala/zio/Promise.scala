package zio

// way to wait for a value to be set
// "box" that contains either an E or an A and can be set exactly once
// box is either empty
// or its full with an E or an A
trait Promise[E, A] {

  // wait for a value to be set and then return it
  def await: ZIO[E, A]

  // set a value
  def complete(zio: ZIO[E, A]): ZIO[Nothing, Unit]

  final def succeed(a: A): ZIO[Nothing, Unit] =
    complete(ZIO.succeed(a))

  final def fail(e: E): ZIO[Nothing, Unit] =
    complete(ZIO.fail(e))
}

object Promise {

  sealed trait State[E, A]

  final case class Empty[E, A](callbacks: List[ZIO[E, A] => Unit]) extends State[E, A]
  final case class Full[E, A](zio: ZIO[E, A])                      extends State[E, A]

  def make[E, A]: ZIO[Nothing, Promise[E, A]] =
    ZIO.succeed {
      val state = new java.util.concurrent.atomic.AtomicReference[State[E, A]](Empty(Nil))
      new Promise[E, A] {
        def await: ZIO[E, A] =
          ZIO.async { cb =>
            val currentState = state.getAndUpdate {
              case Empty(callbacks) => Empty(cb :: callbacks)
              case Full(zio)        => Full(zio)
            }
            currentState match {
              case Empty(_)  => ()
              case Full(zio) => cb(zio)
            }
          }
        def complete(zio: ZIO[E, A]): ZIO[Nothing, Unit] =
          ZIO.succeed {
            val currentState = state.getAndUpdate {
              case Empty(callbacks) => Full(zio)
              case Full(zio) => Full(zio)
            }
            currentState match {
              case Empty(callbacks) => callbacks.foreach(_(zio))
              case Full(zio) => ()
            }
          }
      }
    }
}
