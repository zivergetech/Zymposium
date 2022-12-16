import zio._
import zio.direct._

object Example extends ZIOAppDefault {

  def notAWorkflow() = {
    val as = List(1, 2, 3)
    val bs = List(4, 5, 6)
    val cs = if (as.startsWith(List(2))) {
      val cs = as.zip(bs).map { case (a, b) => a + b }
      println(cs)
      cs
    } else {
      bs.zip(as).map { case (b, a) => b + a }
    }
    cs
  }

  trait ListConfig1
  trait ListConfig2

  val listConfig1Live: ULayer[ListConfig1] =
    ZLayer.succeed(new ListConfig1 {})

  val listConfig2Live: ULayer[ListConfig2] =
    ZLayer.succeed(new ListConfig2 {})

  def getFirstFancyListfromDatabase: ZIO[ListConfig1, Nothing, List[Int]] =
    ZIO.succeed(List(1, 2, 3))

  def getSecondFancyListfromDatabase: ZIO[ListConfig2, Nothing, List[Int]] =
    ZIO.succeed(List(4, 5, 6))

  def printSomething(string: String): ZIO[Any, Nothing, Unit] =
    ZIO.succeed(println(string))

  // val future = Future(42)
  // future.await

  val actuallyIsAWorkflow = defer {
    val as = getFirstFancyListfromDatabase.run // NOT: Runtime.default.unsafe.run(getFirstFancyListfromDatabase)
    val bs = getSecondFancyListfromDatabase.run
    val cs = if (as.startsWith(List(2))) {
      val cs = as.zip(bs).map { case (a, b) => a + b }
      printSomething(cs.toString).run
      cs
    } else {
      bs.zip(as).map { case (b, a) => b + a }
    }
    cs
  }

  val anotherWorklow =
    ZIO.succeed {
      ZIO.succeed(println("Hello World!")) // will never be executed
      42
    }

  trait DBErr extends Throwable
  trait ParseErr extends Throwable
  trait ID
  trait Person

  def parseId(s: String): IO[ParseErr, ID] = ???
  def getPerson(id: ID): IO[DBErr, Person] = ???

  // val t = defer {
  //   val id = parseId("1").run
  //   getPerson(id).run
  // }

  val getNums: ZIO[Any, Nothing, List[Int]] =
    ZIO.succeed(List(1, 2, 3))

  def printNum(n: Int): ZIO[Any, Nothing, Unit] =
    ZIO.succeed(println(n))

  // val x = defer {
  //   for
  //     n <- getNums.run
  //   do if (n % 2 == 0) printNum(n).run // error
  // }

  // val zzz = defer {
  //   getNums.run.withFilter(n => n % 2 == 0).foreach(n => printNum(n).run)
  // }

  val y =
    for n <- List(1, 2, 3)
    if n % 2 == 0 do println(n)

  // for {
  //   n <- List(1, 2, 3)
  // } println(n)

  // List(1, 2, 3).foreach(n => println(n))

  val traditionalWorkflow =
    for {
      as <- ZIO.succeed(List(1, 2, 3))
      bs <- ZIO.succeed(List(4, 5, 6))
      cs <- if (as.startsWith(List(2))) ZIO.succeed(as.zip(bs).map { case (a, b) => a + b }).debug
            else ZIO.succeed(bs.zip(as).map { case (b, a) => b + a })
    } yield ()

  // val somethingSuperLowLeve = 
  //   ZIO.succeed {
  //     var i = 0
  //     while ()
  //   }

  // def doSomething(a: Int)(b: Int): Int =
  //   a + b

  // val increment = doSomething(1)(_)

  val countUp =
    defer.info {
      val ref = Ref.make(0).run
      while (ref.get.run < 10) {
        val increment: Int => Int = { (x: Int) => x + 1 }
        ref.update(increment).run
      }
      ref.get.run
    }

  val run =
    countUp.debug("result")
    // (actuallyIsAWorkflow.race(traditionalWorkflow))
    //   .debug("actuallyIsAWorkflow")
    //   .provide(
    //     listConfig1Live,
    //     listConfig2Live
    //   )
  // actuallyIsAWorkflow: List(5, 7, 9)
}
