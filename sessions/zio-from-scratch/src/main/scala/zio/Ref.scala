package zio

// ZIO equivalent of a Java atomic reference
// communicate information between different fibers
// a very light wrapper around a Java atomic reference
// lets us work with atomic values in a "ZIO idiomatic way"
// "descriptions" instead of actually doing things
// a little higher level operators
// modify ends up being much nicer to work with that the Java API
// available on a cross platform basis

trait Ref[A] {

  def get: ZIO[Nothing, A]

  def set(a: A): ZIO[Nothing, Unit]

  def modify[B](f: A => (B, A)): ZIO[Nothing, B]

  final def update(f: A => A): ZIO[Nothing, Unit] =
    modify(a => ((), f(a)))

  final def updateAndGet(f: A => A): ZIO[Nothing, A] =
    modify { oldValue =>
      val newValue = f(oldValue)
      (newValue, newValue)
    }

  final def getAndUpdate(f: A => A): ZIO[Nothing, A] =
    modify { oldValue =>
      val newValue = f(oldValue)
      (oldValue, newValue)
    }
}

object Ref {

  def make[A](initial: A): ZIO[Nothing, Ref[A]] =
    ZIO.succeed {
      new Ref[A] {
        val ref = new java.util.concurrent.atomic.AtomicReference[A](initial)
        def get: ZIO[Nothing, A] =
          ZIO.succeed(ref.get)
        def set(a: A): ZIO[Nothing, Unit] =
          ZIO.succeed(ref.set(a))
        def modify[B](f: A => (B, A)): ZIO[Nothing, B] =
          ZIO.succeed {
            var loop = true
            var summary: B = null.asInstanceOf[B]
            while (loop) {
              val oldValue = ref.get
              val tuple = f(oldValue)
              summary = tuple._1
              val newValue = tuple._2
              loop = !ref.compareAndSet(oldValue, newValue)
            }
            summary
          }
      }
    }
}