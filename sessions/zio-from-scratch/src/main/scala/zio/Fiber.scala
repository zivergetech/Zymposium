package zio

// ZIO is a blueprint. We can compose it with other blueprints.
// Modify the blueprint to create a new blueprint.
// retry, timed, etc.
//
// Fiber is an executing ZIO blueprint.
trait Fiber[+E, +A] {

  // wait for execution to complete, and return the result... as a blueprint
  // semantic blocking. it will not ACTUALLY block an OS thread.
  def await: ZIO[Nothing, Exit[E, A]]

  // send the interrupt signal to fiber and then return immediately
  def interruptFork: ZIO[Nothing, Unit]

  final def join: ZIO[E, A] =
    await.flatMap(ZIO.done)

  // wait for interruption to be processed by fiber
  def interrupt: ZIO[Nothing, Exit[E, A]] =
    interruptFork *> await
}