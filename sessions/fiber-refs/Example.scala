import zio._

object Example extends ZIOAppDefault {

  // What is a FiberRef?
  // Why would we use one?

  // Patch theory

  // FiberRef - ZIO's equivalent of a ThreadLocal in Java
  // Just like a Threadlocal has a different copy for every thread
  // A FiberRef has a different copy for every fiber

  // Use a FiberRef whenever you want "context" that is not reflected in the environment type

  // What do we mean by context?

  // Context is all the information our workflow needs to run that isn't reflected in the arguments
  // to the function that creates it

  // Some database interface
  // Some logger
  // Some metrics provider
  // Some logging span
  // Some trace
  // Some configuration options (like how many times we should retry)

  // Two ways to represent context
  // 1. ZIO Environment type
  // 2. FiberRef - "context" is not reflected in our type signature anymore

  // Environment type is "explicit"
  // Makes the requirement for context very clear to the user and what that context looks like
  // This is particularly good for cases where there isn't a sensible "default" value
  // Or where the context is "key" to the logic of the workflow

  // FiberRef is more "implicit"
  // The context requirement is not reflected in the type signature
  // Keeps the type signature "clean" from "implementation details" that we want to be
  // able to change if needed but we don't want to put so from and center for our users
  // Need to have some sensible "default" value for the context

  final case class User(id: Long)

  // A workflow that requires services R and can fail with an E or succeed with an A
  // trait ZIO[-R, +E, +A]

  trait Database {
    def persistUser(id: Long): ZIO[Any, Nothing, Unit]
  }

  object Database {
    val dummy: ZLayer[Any, Nothing, Database] =
      ZLayer.succeed {
        new Database {
          def persistUser(id: Long): ZIO[Any, Nothing, Unit] =
            ZIO.debug(s"Persisting user $id")
        }
      }

    def persistUser(id: Long): ZIO[Database, Any, Unit] =
      ZIO.serviceWithZIO[Database](_.persistUser(id))
  }

  trait Logging {
    def log(message: String): ZIO[Any, Nothing, Unit]
  }

  object Logging {

    private val default = new Logging {
      def log(message: String): ZIO[Any, Nothing, Unit] =
        Console.printLine("DEFAULT LOGGER: " + message).orDie
    }

    val silent = new Logging {
      def log(message: String): ZIO[Any, Nothing, Unit] =
        ZIO.unit
    }

    val currentLogger: FiberRef[Logging] =
      Unsafe.unsafe { implicit unsafe =>
        FiberRef.unsafe.make(default)
      }

    def log(message: String): ZIO[Any, Nothing, Unit] =
      currentLogger.get.flatMap(_.log(message))
  }

  // How many times we should retry the database operation if it fails
  val retries: FiberRef[Int] =
    Unsafe.unsafe { implicit unsafe =>
      FiberRef.unsafe.make(3)
    }

  // Duration in milliseconds between each retry
  val retryInterval: FiberRef[Int] =
    Unsafe.unsafe { implicit unsafe =>
      FiberRef.unsafe.make(100)
    }

  final case class RetryConfig(retries: Int, retryInterval: Int)

  val retryConfig: FiberRef[RetryConfig] =
    Unsafe.unsafe { implicit unsafe =>
      FiberRef.unsafe.makePatch(
        RetryConfig(3, 100),
        Differ
          .update[Int]
          .zip(Differ.update[Int])
          .transform(RetryConfig.apply, retryConfig => (retryConfig.retries, retryConfig.retryInterval)),
        Differ
          .update[Int]
          .zip(Differ.update[Int])
          .transform(RetryConfig.apply, retryConfig => (retryConfig.retries, retryConfig.retryInterval))
          .empty
      )
    }

  def withRetries(n: Int): ZIO[Any, Nothing, Unit] =
    for {
      config <- retryConfig.get
      _      <- retryConfig.set(config.copy(retries = n))
    } yield ()

  def withRetryInterval(n: Int): ZIO[Any, Nothing, Unit] =
    for {
      config <- retryConfig.get
      _      <- retryConfig.set(config.copy(retryInterval = n))
    } yield ()

  def persistUser(user: User): ZIO[Database, Any, Unit] =
    for {
      _ <- Logging.log(s"About to persist user ${user.id}")
      _ <- Database.persistUser(user.id)
    } yield ()

  // when we set the FiberRef to two different values the last one wins
  // when we join a fiber the parent fiber is going to get the FiberRef value of the child it joined
  // fork join identity
  // making our program more parallel shouldn't change the meaning of the rest of our program

  // Parent fiber: Map(retries -> 3, retryInterval -> 100)
  // Left fiber: Map(retries -> 5, retryInterval -> 100)
  // Right fiber: Map(retries -> 3, retryInterval -> 200)

  // "Patch theory"
  // Github
  // version control system

  // Patch

  // Parent fiber joins the left fiber: Map(retries -> 5, retryInterval -> 100)
  // Parent fiber joins the right fiber: Map(retries -> 3, retryInterval -> 200)

  // How do we solve this with patch theory

  // Parent fiber: Map(retries -> 3, retryInterval -> 100)
  // Left fiber...
  // Map(retries -> 3, retryInterval -> 100)
  // Map(retries -> 5, retryInterval -> 100)
  // Patch("set retries to five")
  // diff
  // Right fiber: Patch("set retryInterval to 200")

  // Parent fiber join the left fiber...
  // Map(retries -> 3, retryInterval -> 100)
  // Patch("set retries to five")
  // Map(retries -> 5, retryInterval -> 100)
  // patch

  // Parent fiber join the right fiber...
  // Map(retries -> 5, retryInterval -> 100)
  // Patch("set retryInterval to 200")
  // Map(retries -> 5, retryInterval -> 200)
  // patch

  val run =
    for {
      fiberRef <- FiberRef.make(0)
      _        <- withRetries(5).zipPar(withRetryInterval(200))
      _        <- retryConfig.get.debug("retryConfig")
    } yield ()

// Map(retries -> 5, retryInterval -> 200)
// Map(retries -> 3, retryInterval -> 200)

// retryConfig: Map(retries -> 5, retryInterval -> 200)

// RetryConfig(5,200)
}
