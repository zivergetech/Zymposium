package zio

sealed trait FiberRef[A] { self =>
  def default: A

  def modify[B](f: A => (B, A)): ZIO[Nothing, B] =
    ZIO.ModifyFiberRefs { fiberRefs =>
      val currentValue        = fiberRefs.getOrElse(self, default).asInstanceOf[A]
      val (summary, newValue) = f(currentValue)
      (summary, fiberRefs.updated(self, newValue))
    }

  def set(a: A): ZIO[Nothing, Unit] =
    modify(_ => ((), a))

  def update(f: A => A): ZIO[Nothing, Unit] =
    modify(a => ((), f(a)))

  def get: ZIO[Nothing, A] =
    modify(a => (a, a))

}

object FiberRef {
  def make[A](initial: A): ZIO[Nothing, FiberRef[A]] =
    ZIO.succeed(new FiberRef[A] {
      def default: A = initial
    })
}