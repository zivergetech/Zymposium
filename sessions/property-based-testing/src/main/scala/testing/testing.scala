import zio._
import zio.stream._

package object testing {

  // generate a number of samples from our generator
  // run the predicate for each of them
  // if the predicate is satisfied for all of them we succeed
  // otherwise we fail on the first failure

  def test[A](a: A)(f: A => Boolean): ZIO[Any, Nothing, TestResult] =
    ZIO.succeed {
      if (f(a)) TestResult.Success
      else TestResult.Failure(a.toString)
    }

  def report(testResult: TestResult): ZIO[Any, Nothing, Unit] =
    testResult match {
      case TestResult.Success => Console.printLine("The test passed!").orDie // number of tests run, name of test...
      case TestResult.Failure(inputs) => Console.printLine(s"The test failed on inputs: $inputs").orDie
    }

  // TestConfig
  //  how many samples to run
  //  "size"
  //  "shrinking"
  //  repeat or retry tests

  def check[A](gen: Gen[A])(f: A => Boolean): ZIO[Any, Nothing, TestResult] =
    ZIO.withRandom(Random.RandomScala(new scala.util.Random(42))) {
      ZStream.repeatZIO(gen.sample.debug.flatMap(a => test(a)(f))).take(100).runCollect.map { results =>
        results.collectFirst {
          case TestResult.Failure(failure) => TestResult.Failure(failure)
        }.getOrElse(TestResult.Success)
      }
    }

  def check[A, B](first: Gen[A], second: Gen[B])(f: (A, B) => Boolean): ZIO[Any, Nothing, TestResult] =
    check(first <*> second)(f.tupled)

  def check[A, B, C](first: Gen[A], second: Gen[B], third: Gen[C])(f: (A, B, C) => Boolean): ZIO[Any, Nothing, TestResult] =
    check(first <*> second <*> third) { case ((a, b), c) => f(a, b, c) }

  def check[A, B, C, D](first: Gen[A], second: Gen[B], third: Gen[C], fourth: Gen[D])(f: (A, B, C, D) => Boolean): ZIO[Any, Nothing, TestResult] =
    check(first <*> second <*> third <*> fourth) { case (((a, b), c), d) => f(a, b, c, d) }

  def check[A, B, C, D, E](first: Gen[A], second: Gen[B], third: Gen[C], fourth: Gen[D], fifth: Gen[E])(f: (A, B, C, D, E) => Boolean): ZIO[Any, Nothing, TestResult] =
    check(first <*> second <*> third <*> fourth <*> fifth) { case ((((a, b), c), d), e) => f(a, b, c, d, e) }
}