package zio

import scala.annotation.tailrec

object Example extends App {

  val randomIntWorkflow       = ZIO.succeed(scala.util.Random.nextInt(99))
  def printWorkflow(any: Any) = ZIO.succeed(println(s"I got the number ${any}"))

  val myThirdWorkflow =
    for {
      int <- randomIntWorkflow
      _   <- printWorkflow(int)
    } yield ()

  def speakWithDelay(text: String)(delay: Long): ZIO[Nothing, Unit] =
    ZIO.succeed {
      Thread.sleep(delay) // ZIO.sleep would be only "semantically" blocking
      println(text)
    }

  val myParallelWorkflow: ZIO[Nothing, Unit] =
    speakWithDelay("Hello")(3000)
      .zipWithPar(speakWithDelay("World")(5000))((_, _) => ())

  // 5 secs!
  // myThirdWorkflow.unsafeRunSync()
  // myParallelWorkflow.unsafeRunSync()

  // interrupt
  val myFailingWorkflow =
    ZIO
      .succeed("yay")
      .flatMap(a => ZIO.succeed(println("success: " + a)))
      .flatMap(_ => ZIO.fail("whoops"))
      .flatMap(_ => ZIO.succeed(println("Done something else")))
      .flatMap(_ => ZIO.succeed(println("Done something else")))
      .catchAll(e => ZIO.succeed(println(s"Error: ${e}")))
  // 1. print success yay
  // 2. print Error: whoops

  // myFailingWorkflow.unsafeRunSync()

  // classical imperative code
  // val int = getInt
  // printInt(int)

  //randomIntWorkflow.unsafeRunSync()

  def debug(any: Any): ZIO[Nothing, Unit] =
    ZIO.succeed(println(s"debug: $any"))

  // - interrupt
  // - succeed
  val causeExample =
    ZIO
      .succeed(throw new RuntimeException("boom"))
      .catchAllCause(cause => debug(s"caught cause $cause"))

  // causeExample.unsafeRunSync()

  val selfInterruptionExample =
    ZIO.interrupt

  // external interruption

  // val exit = selfInterruptionExample.unsafeRunSync()
  // println(exit)

  val interruptionExample1 =
    for {
      _     <- debug("Starting")
      fiber <- debug(".").forever.fork
      _     <- debug("Forked")
      _     <- speakWithDelay("Hello")(3000)
      _     <- fiber.interrupt
    } yield ()

  val example =
    for {
      fiber <- ZIO.never.fork
      _     <- debug("Forked")
      _     <- speakWithDelay("Hello")(3000)
      _     <- fiber.interrupt
    } yield ()

  // val exit = interruptionExample1.unsafeRunSync()

  val child =
    for {
      _ <- ZIO.uninterruptible {
             speakWithDelay("child is running in uninterruptible region")(2000) *>
               debug(".").repeatN(1000)
           }
      _ <- debug("*").forever
    } yield ()

  val initialRegionsExample =
    for {
      fiber <- child.fork
      _     <- speakWithDelay("waiting")(1000)
      _     <- debug("about to interrupt child")
      _ <- fiber.interrupt // interrupt = interruptFork *> await
      _ <- debug("done interrupting child")
    } yield ()

  // val exit = initialRegionsExample.unsafeRunSync()

  // println("exit = " + exit)

  def sleep(ms: Int) =
    ZIO.succeed {
      Thread.sleep(ms)
    }

  val myResource =
    ZIO.acquireReleaseExitWith {
      ZIO.succeed(println("acquired"))
    } { (_, exit) =>
      ZIO.succeed(println(s"released with $exit"))
    } { _ =>
      ZIO.succeed(println("using")) *> ZIO.never
    }

  val myResourceExample =
    for {
      fiber <- myResource.fork
      _     <- sleep(1000)
      _     <- fiber.interrupt
      _     <- ZIO.succeed(println("done"))
    } yield ()

  // myResourceExample.unsafeRunSync()

  def useFiberRef(fiberRef: FiberRef[Int]) =
    for {
      _ <- fiberRef.update(_ + 1)
      _ <- fiberRef.update(_ + 1)
      value <- fiberRef.modify { a =>
                 (a, a + 1)
               }
      _ <- ZIO.succeed(println(value))
    } yield ()

  val myFiberRefExample =
    for {
      fiberRef <- FiberRef.make(0)
      _        <- fiberRef.set(1)
      f1 <- useFiberRef(fiberRef).fork // 2
      f2 <- useFiberRef(fiberRef).fork // 2
      _ <- useFiberRef(fiberRef)       // 3
    } yield ()

  myFiberRefExample.unsafeRunSync()
}
