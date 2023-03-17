package testing

import zio._

final case class Gen[+A](sample: ZIO[Any, Nothing, A]) { self =>

  def <*>[B](that: Gen[B]): Gen[(A, B)] =
    zip(that)

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def zip[B](that: Gen[B]): Gen[(A, B)] =
    zipWith(that)((_, _))

  def zipWith[B, C](that: Gen[B])(f: (A, B) => C): Gen[C] =
    self.flatMap(a => that.map(b => f(a, b)))
}

object Gen {

  // constructors of generators for different types

  val boolean: Gen[Boolean] =
    fromRandom(_.nextBoolean)

  def collectAll[A](gens: Iterable[Gen[A]]): Gen[List[A]] =
    Gen(ZIO.collectAll(gens.toList.map(_.sample)))

  def constant[A](a: A): Gen[A] =
    Gen(ZIO.succeed(a))

  // Gen.elements(1, 2, 3)
  // Gen.elements(1)
  // Gen.elements(1, 2, 3, 4, 5)
  // List(1, 2, 3, 4, 5)

  def elements[A](as: A*): Gen[A] = {
    val x = as // Seq linear collection // ArraySeq(Fido, Rex, Snoopy)
    Gen.intBounded(as.length).map { index =>
      as(index)
    }
  }

  def oneOf[A](as: Gen[A]*): Gen[A] =
    fromRandom(_.nextIntBounded(as.size)).flatMap(as)

  def listOfN[A](n: Int)(gen: Gen[A]): Gen[List[A]] =
    collectAll(List.fill(n)(gen))

  // uses some default size range
  def listOf[A](gen: Gen[A]): Gen[List[A]] =
    Gen.intBetween(0, 10).flatMap { n => Gen.listOfN(n)(gen) }

  val char: Gen[Char] =
    intBounded(128).map(_.toChar)

  val int: Gen[Int] =
    fromRandom(_.nextInt)

  def intBounded(max: Int): Gen[Int] =
    fromRandom(_.nextIntBounded(max))

  def intBetween(min: Int, max: Int): Gen[Int] =
    fromRandom(_.nextIntBetween(min, max))

  def fromRandom[A](f: Random => ZIO[Any, Nothing, A]): Gen[A] =
    Gen(ZIO.randomWith(f))

  def stringN(n: Int): Gen[String] =
    listOfN(n)(char).map(_.mkString)

  def string: Gen[String] =
    listOf(char).map(_.mkString)
}