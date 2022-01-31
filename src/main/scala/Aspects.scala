import zio._
import java.io.IOException

object Aspects {

  // What we're doing
  // ================
  // Returning an Int
  // Transforming an Int to a String

  // How we're doing it
  // ==================
  // What executor are we running on?
  // What yield op count are we using before we yield in the runtime?
  // What logging context are we in?
  // What parallelism are we using?

  // Streams
  // =======
  // What chunk size are we using?

  // Testing
  // =======
  // How many times are we repeating tests?
  // How large are our property based values
  // How many samples are we looking at?
  // Should we log console output?

  val myZIO: ZIO[Any, Nothing, Int] =
    ???

  // ZIO[R, E, A]

  // Lower is most specific, lowest possible type is Nothing
  // Any is the most general, highest possible type is Any

  trait ZAspect[+LowerR, -UpperR, +LowerE, -UpperE, +LowerA, -UpperA] {
    def apply[
        R >: LowerR <: UpperR,
        E >: LowerE <: UpperE,
        A >: LowerA <: UpperA
    ](zio: ZIO[R, E, A]): ZIO[R, E, A]
  }

  trait ZAspectComplex[+LowerR, -UpperR, +LowerE, -UpperE, +LowerA, -UpperA] {
    type OutR[R]
    type OutE[E]
    type OutA[A]
    def apply[
        R >: LowerR <: UpperR,
        E >: LowerE <: UpperE,
        A >: LowerA <: UpperA
    ](zio: ZIO[R, E, A]): ZIO[OutR[R], OutE[E], OutA[A]]
  }

  type ??? = Nothing

  trait Logging {
    def logLine(line: String): ZIO[Any, Nothing, Unit]
    def withAnnotation[R, E, A](key: String, value: String)(
        zio: ZIO[R, E, A]
    ): ZIO[R, E, A]
  }

  object Logging {
    def withAnnotation[R, E, A](key: String, value: String)(
        zio: ZIO[R, E, A]
    ): ZIO[R with Logging, E, A] =
      ZIO.serviceWithZIO[Logging](_.withAnnotation(key, value)(zio))
  }

  object ZAspect {
    def onExecutor(
        executor: Executor
    ): ZAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
      new ZAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
        def apply[
            R >: Nothing <: Any,
            E >: Nothing <: Any,
            A >: Nothing <: Any
        ](zio: ZIO[R, E, A]): ZIO[R, E, A] =
          zio.onExecutor(executor)
      }

    // Does what I'm doing have environment requirements - use UpperR
    // Does it introduce new failure modes - use LowerE
    // Does it need to know something about error or value types - use UpperE or UpperA

    def annotate(
        key: String,
        value: String
    ): ZAspect[Nothing, Logging, Nothing, Any, Nothing, Any] =
      new ZAspect[Nothing, Logging, Nothing, Any, Nothing, Any] {

        def apply[
            R >: Nothing <: Logging,
            E >: Nothing <: Any,
            A >: Nothing <: Any
        ](zio: ZIO[R, E, A]): ZIO[R, E, A] =
          Logging.withAnnotation(key, value)(zio)
      }

    // ZAspect.make(Logging.withAnnotation(key, value)(_))

    /** An aspect that renders the success value of the effect to the console.
      */
    def renderConsole
        : ZAspect[Nothing, Console, IOException, Any, Nothing, Any] =
      new ZAspect[Nothing, Console, IOException, Any, Nothing, Any] {

        def apply[
            R >: Nothing <: Console,
            E >: IOException <: Any,
            A >: Nothing <: Any
        ](zio: ZIO[R, E, A]): ZIO[R, E, A] =
          zio.tap(a => Console.printLine(a))
      }

    def updateTemperature =
      new ZAspect[Nothing, Any, Nothing, Any, Any, Double] {

        def apply[
            R >: Nothing <: Any,
            E >: Nothing <: Any,
            A >: Nothing <: Double
        ](zio: ZIO[R, E, A]): ZIO[R, E, A] =
          zio.tap(updateTemperatureMetric)
      }
  }

  sealed trait Result

  final case class ValueFound(value: Double) extends Result
  case object Default

  def updateTemperatureMetric(value: Double): UIO[Unit] =
    ???

  // ZIO[-R, +E, +A]

  implicit class ZAspectSyntax[R, E, A](private val zio: ZIO[R, E, A])
      extends AnyVal {
    def @@@[
        LowerR <: UpperR,
        UpperR <: R,
        LowerE >: E,
        UpperE >: LowerE,
        LowerA >: A,
        UpperA >: LowerA
    ](
        aspect: ZAspect[LowerR, UpperR, LowerE, UpperE, LowerA, UpperA]
    ): ZIO[UpperR, LowerE, LowerA] =
      aspect(zio)
  }

  val myAspect2 = ZAspect.annotate("foo", "bar")

  val myZIO3: ZIO[Any, Nothing, Int] =
    ???

  val myZIO4 =
    myZIO3 @@@ ZAspect.renderConsole

  val myZIO5: ZIO[Logging, Nothing, Int] =
    myZIO3

  // val myZIO4: ZIO[Logging, Nothing, Int] = myAspect2(myZIO3)

  val myAspect =
    ZIOAspect.onExecutor(???) @@ ZIOAspect.parallel(8)

  val myZIO2: ZIO[Any, Nothing, String] =
    myZIO.map(_.toString).flatMap(a => ZIO.succeed(a + "!")) @@ myAspect
}

// // Sub <: Super
// // More Specific
// // More General
// trait Animal
// trait Dog extends Animal
// trait HairlessFlamingDog extends Dog

// // trait Logging
// // trait Database

// // information
// // Any is a type that has no information

// // trait MyAny {}

// // trait MyNothing {
// //   def thePriceOfBitcoinTomorrow: Double =
// //     ???
// // }

// // isSubtype[Logging with Database, Logging]
// // isSuperType[Logging, Logging with Database]
// // isSameType[Database with Logging, Logging with Database]
// // isSubtype[HairlessFlamingDog, Dog]

// // def isSubtype[Left, Right](implicit ev: Left <:< Right): Unit =
// //   ()

// // def isSuperType[Left, Right](implicit ev: Right <:< Left): Unit =
// //   ()

// // def isSameType[Left, Right](implicit ev: Right <:< Left, ev2: Left <:< Right): Unit =
// //   ()

// // Logging with Database <: Logging <: Any
