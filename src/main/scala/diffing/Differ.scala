package diffing

import diffing.DiffResult.Labeled

import language.experimental.macros, magnolia1._

trait Show[A] {
  def show(a: A): String
}

object DeriveDiffer {
  type Typeclass[A] = Differ[A]

  def join[A](ctx: CaseClass[Differ, A]): Differ[A] =
    new Differ[A] {
      override def diff(a1: A, a2: A): DiffResult =
        if (a1 == a2) DiffResult.Identical(a1)
        else {
          val labeleds: List[Labeled] = ctx.parameters.toList.map { param =>
            Labeled.Diff(param.label, param.typeclass.diff(param.dereference(a1), param.dereference(a2)))
          }
          DiffResult.Nested(ctx.typeName.full, labeleds)
        }
    }

  def split[A](ctx: SealedTrait[Differ, A]): Differ[A] =
    new Differ[A] {
      override def diff(a1: A, a2: A): DiffResult =
        if (a1 == a2) DiffResult.Identical(a1)
        else
          ctx.split(a1) { subtype =>
            val cast = subtype.cast
            (a1, a2) match {
              case (cast(a1), cast(a2)) => subtype.typeclass.diff(a1, a2)
              case _                    => DiffResult.Different(a1, a2)
            }
          }

    }

  implicit def gen[A]: Differ[A] = macro Magnolia.gen[A]
}

// Today's Topic: Differ
// - functional domain
// - type-class
// - encoder/decoder
// - magnolia
// - zio-json
// - JSON diffing

object DiffResult {
  final case class Identical(value: Any)                        extends DiffResult
  final case class Different(left: Any, right: Any)             extends DiffResult
  final case class Nested(name: String, labeled: List[Labeled]) extends DiffResult

  sealed trait Labeled extends Product with Serializable {
    def label: String
  }

  object Labeled {
    final case class Added(label: String, value: Any)            extends Labeled
    final case class Removed(label: String, value: Any)          extends Labeled
    final case class Diff(label: String, diffResult: DiffResult) extends Labeled
  }

  val map1: Map[String, Map[String, Int]] =
    Map(
      "first"  -> Map("one" -> 1, "two" -> 2, "three" -> 3),
      "second" -> Map.empty,
      "fifth"  -> Map("good" -> 300)
    )

  val map2: Map[String, Map[String, Int]] =
    Map(
      "first"  -> Map("one" -> 1, "two" -> 3, "four" -> 4),
      "second" -> Map.empty,
      "fourth" -> Map("hello" -> 300)
    )

  def main(args: Array[String]): Unit =
    println(Differ.diff(map1, map2).render)
}

sealed trait DiffResult extends Product with Serializable { self =>

  def indent(string: String): String =
    string.linesIterator.map(s => s"  $s").mkString("\n")

  def red(string: String): String =
    s"\u001b[31m$string\u001b[0m"

  def green(string: String): String =
    s"\u001b[32m$string\u001b[0m"

  def cyan(string: String): String =
    s"\u001b[36m$string\u001b[0m"

  def render: String = self match {
    case DiffResult.Identical(value)       => s"$value"
    case DiffResult.Different(left, right) => cyan(s"$left -> $right")
    case DiffResult.Nested(name, labeled) =>
      val rendered = labeled.map {
        case Labeled.Added(label, value)     => green(s"+ $label = $value")
        case Labeled.Removed(label, value)   => red(s"- $label = $value")
        case Labeled.Diff(label, diffResult) => s"$label = ${diffResult.render}"
      }.mkString(",\n")
      s"""
$name(
${indent(rendered)}
)
         """.trim
  }

}

trait Differ[A] {
  def diff(a1: A, a2: A): DiffResult
}

object Differ {

  def diff[A](a1: A, a2: A)(implicit differ: Differ[A]): DiffResult =
    differ.diff(a1, a2)

  implicit val stringDiffer: Differ[String] =
    new Differ[String] {
      override def diff(a1: String, a2: String): DiffResult =
        if (a1 == a2) DiffResult.Identical(a1)
        else DiffResult.Different(a1, a2)
    }

  implicit val intDiffer: Differ[Int] =
    new Differ[Int] {
      override def diff(a1: Int, a2: Int): DiffResult =
        if (a1 == a2) DiffResult.Identical(a1)
        else DiffResult.Different(a1, a2)
    }

  implicit val booleanDiffer: Differ[Boolean] =
    new Differ[Boolean] {
      override def diff(a1: Boolean, a2: Boolean): DiffResult =
        if (a1 == a2) DiffResult.Identical(a1)
        else DiffResult.Different(a1, a2)
    }

  implicit def mapDiffer[K, V](implicit differ: Differ[V]): Differ[Map[K, V]] = new Differ[Map[K, V]] {
    override def diff(oldMap: Map[K, V], newMap: Map[K, V]): DiffResult = {
      if (oldMap == newMap) return DiffResult.Identical(oldMap)

      val newKeys = oldMap.keySet
      val oldKeys = newMap.keySet

      val addedKeys   = oldKeys -- newKeys
      val removedKeys = newKeys -- oldKeys
      val allKeys     = newKeys union oldKeys

      val labeled: List[Labeled] =
        allKeys.toList.map { key =>
          if (addedKeys.contains(key))
            Labeled.Added(key.toString, newMap(key))
          else if (removedKeys.contains(key))
            Labeled.Removed(key.toString, oldMap(key))
          else
            Labeled.Diff(key.toString, differ.diff(oldMap(key), newMap(key)))
        }

      DiffResult.Nested("Map", labeled)
    }
  }
}

final case class Coord(x: Int, y: Int, z: Int)
final case class Person(name: String, age: Int, job: Job, coord: Coord)

sealed trait Job extends Product with Serializable

object Job {
  final case class Chef(numberOfBananas: Int)                              extends Job
  final case class Waiter(numberOfDishes: Int)                             extends Job
  final case class Programmer(favoriteLanguage: String, numberOfBugs: Int) extends Job
}

object Person {
  import DeriveDiffer.gen

  def main(args: Array[String]): Unit =
    println(
      Differ
        .diff(
          Person("John", 30, Job.Programmer("Scala", 28), Coord(1, 2, 3)),
          Person("Adam", 35, Job.Programmer("Scala", 3), Coord(4, 2, 6))
        )
        .render
    )
}
