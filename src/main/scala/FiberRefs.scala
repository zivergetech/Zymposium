import zio._

object FiberRefs extends ZIOAppDefault {
  // Sponsored by Le Croix

  // ZIO equivalent of Java ThreadLocal

  // Whenever you have some kind of scoped information or context
  // think about a FiberRef
  // FiberRefs in the environment are a particularly nice way to do this

  // Refs and FiberRefs - how are they different?

  // Ref - one variable shared between different fibers
  // Communicating between fibers
  // Ref represents logging context
  // Fiber1 - Make ("Zymposium")
  // Fiber2                 - Set("Kit")           - Log        - Set("Zymposium")
  // Fiber3                            Set("Adam")        - Log - Set("Zymposium")

  // FiberRef - each fiber gets its own copy of the FiberRef
  // Maintaining some scoped state or context
  // When we fork a fiber it gets an initial value equal to its parent's value
  // When we join a fiber (by default) we get the value of the fiber we joined
  // Fiber1 - Make ("Zymposium")
  // Fiber2                 - Set("Kit")           - Log        - Set("Zymposium")
  // Fiber3                            Set("Adam")        - Log - Set("Zymposium")

  val refExample: ZIO[Any, Nothing, Unit] =
    for {
      ref <- Ref.make("Zymposium")
      kit = ref.set("Kit") *> ref.get.debug("KIT")
      adam = ref.set("Adam") *> ref.get.debug("ADAM")
      _ <- kit.zipPar(adam)
      _ <- ref.get.debug("ZYMOSPHERE")
    } yield ()

  lazy val fiberRefExample: ZIO[Any, Nothing, Unit] =
    ZIO.scoped {
      for {
        ref <- FiberRef.make("Zymposium")
        kit = ref.locally("Kit")(ref.get.debug("KIT"))
        adam = ref.locally("Adam")(ref.get.debug("ADAM"))
        _ <- kit.zipPar(adam)
        _ <- ref.get.debug("ZYMOSPHERE")
      } yield ()
    }

  lazy val forkedFibersExample: ZIO[Any, Nothing, Unit] = {
    ZIO.scoped {
      for {
        ref <- FiberRef.make[Map[String, String]](Map.empty, join = _ ++ _)
        kit = ref.set(Map("Kit" -> "Cool"))
        adam = ref.set(Map("Adam" -> "Nice!"))
        _ <- kit.zipPar(adam)
        _ <- ref.get.debug("Annotation Map")
      } yield ()
    }
  }

  // Log annotations
  // Log levels
  // Managed resource finalizers
  // Settings for whether to print to the console
  // The number of property based tests
  // Size of generated values
  // Parallelism!

  // Some context or state that we're interested in
  // We want to be able to modify that context or state locally

  trait Logging {
    def logAnnotate[R, E, A](key: String, value: String)(
        zio: ZIO[R, E, A]
    ): ZIO[R, E, A]
    def logLine(line: String): ZIO[Any, Nothing, Unit]
  }

  case class ConsoleLogging(
      console: Console,
      annotations: FiberRef[Map[String, String]]
  ) extends Logging {

    def logAnnotate[R, E, A](key: String, value: String)(
        zio: ZIO[R, E, A]
    ): ZIO[R, E, A] =
      for {
        map <- annotations.get
        a <- annotations.locally(map.updated(key, value))(zio)
      } yield a

    def logLine(line: String): ZIO[Any, Nothing, Unit] =
      for {
        anns <- annotations.get
        formatted = anns.map { case (k, v) => s"$k: $v" }.mkString(", ")
        _ <- console.printLine(s"[LOG] {$formatted} $line").orDie
      } yield ()

  }

  object Logging {

    val live = ZLayer {
      for {
        annotations <- FiberRef.make(Map.empty[String, String])
        console <- ZIO.service[Console]
      } yield ConsoleLogging(console, annotations)
    }

    // Accessors
    def logLine(line: String): ZIO[Logging, Nothing, Unit] =
      ZIO.service[Logging].flatMap(_.logLine(line))

    def logAnnotate[R, E, A](key: String, value: String)(
        zio: ZIO[R, E, A]
    ): ZIO[Logging with R, E, A] =
      ZIO.service[Logging].flatMap(_.logAnnotate(key, value)(zio))
  }

  def annotated(
      key: String,
      value: String
  ): ZIOAspect[Nothing, Logging, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Logging, Nothing, Any, Nothing, Any] {
      def apply[R <: Logging, E, A](zio: ZIO[R, E, A])(implicit
          trace: ZTraceElement
      ): ZIO[R, E, A] =
        Logging.logAnnotate(key, value)(zio)
    }

  trait ZIOAspect1[+LowerR, -UpperR, +LowerE, -UpperE, +LowerA, -UpperA] {
    def apply[
        R >: LowerR <: UpperR,
        E >: LowerE <: UpperE,
        A >: LowerA <: UpperA
    ](zio: ZIO[R, E, A])(implicit trace: ZTraceElement): ZIO[R, E, A]
  }

  val loggingExample =
    for {
      _ <- Logging.logLine("Starting program")
      _ <- Logging.logAnnotate("Kit", "Cool") {
        Logging.logAnnotate("Adam", "Nice!") {
          Logging.logLine("Hello")
        } *>
          Logging.logLine("Howdy")
      } @@ annotated("Zymposium", "Zymposium")
      _ <- Logging.logLine("All done")
    } yield ()

  val currentParallelism: FiberRef[Option[Int]] =
    Runtime.default.unsafeRun(ZIO.scoped(FiberRef.make[Option[Int]](None)))

  val run =
    loggingExample.provide(Logging.live, Console.live, Scope.default)

}
