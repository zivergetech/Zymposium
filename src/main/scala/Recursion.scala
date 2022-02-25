// ZYMPOSIUM
// "Recursion Schemes"
// 2-25-2022
import Recursion.Tree.Leaf
import Recursion.Tree.Branch
import scala.collection.View
import Recursion.Advanced.Cause.FailCase

object Recursion extends App {

  // Handler for each "case" of the Option
  // Take the information in each case to a "summary" value (Z)
  // None
  // Some(A)
  def foldOption[A, Z](option: Option[A])(none: () => Z, some: A => Z): Z =
    option match {
      case None    => none()
      case Some(a) => some(a)
    }

  // folding with the corresponding constructors == identity
  def identityOption[A](option: Option[A]): Option[A] =
    foldOption(option)(() => None, Some(_))

  // Handler for each "case" of the Either
  // Take the information in each case to a "summary" value (Z)
  // Left(A)
  // Right(B)
  def foldEither[A, B, Z](
      either: Either[A, B]
  )(left: A => Z, right: B => Z): Z =
    either match {
      case Left(a)  => left(a)
      case Right(b) => right(b)
    }

  // folding with the corresponding constructors == identity
  def identityEither[A, B](either: Either[A, B]): Either[A, B] =
    foldEither(either)(Left(_), Right(_))

  // Handler for each "case" of the Either
  // Take the information in each case to a "summary" value (Z)
  // Inductive cases become summary type
  // Nil
  // ::(A, Z)
  def foldList[A, Z](list: List[A])(nil: () => Z, cons: (A, Z) => Z): Z =
    list match {
      case Nil    => nil()
      case h :: t => cons(h, foldList(t)(nil, cons))
    }

  // folding with the corresponding constructors == identity
  def identityList[A](list: List[A]): List[A] =
    foldList[A, List[A]](list)(() => Nil, (h, t) => h :: t)

  type ??? = Nothing

  // I'm a treep
  sealed trait Tree[+A] { self =>
    def fold[Z](leaf: A => Z)(branch: (Z, A, Z) => Z): Z =
      self match {
        case Leaf(value) =>
          leaf(value)
        case Branch(left, middle, right) =>
          branch(left.fold(leaf)(branch), middle, right.fold(leaf)(branch))
      }

    def leaves: Int = fold(_ => 1)((l, _, r) => l + r)

    import scala.math.Numeric.Implicits.infixNumericOps

    def sum[A1 >: A](implicit num: Numeric[A1]): A1 =
      fold[A1](identity)((l, m, r) => l + m + r)
  }

  object Tree {
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], middle: A, right: Tree[A])
        extends Tree[A]
  }

  object Simple {
    import Cause._

    sealed trait Cause[+E] { self =>

      // Combining two different concepts:
      // 1. The concept of recursion
      // 2. The structure of the specific data

      def fold[Z](
          failCase: E => Z,
          dieCase: Throwable => Z,
          bothCase: (Z, Z) => Z,
          thenCase: (Z, Z) => Z,
          tracedCase: (Z, Trace) => Z
      ): Z =
        self match {
          case Fail(e)        => failCase(e)
          case Die(throwable) => dieCase(throwable)
          case Both(left, right) =>
            bothCase(
              left.fold(failCase, dieCase, bothCase, thenCase, tracedCase),
              right.fold(failCase, dieCase, bothCase, thenCase, tracedCase)
            )
          case Then(left, right) =>
            thenCase(
              left.fold(failCase, dieCase, bothCase, thenCase, tracedCase),
              right.fold(failCase, dieCase, bothCase, thenCase, tracedCase)
            )
          case Traced(cause, trace) =>
            tracedCase(
              cause.fold(failCase, dieCase, bothCase, thenCase, tracedCase),
              trace
            )

        }

      def failures: List[E] =
        fold[List[E]](
          List(_),
          _ => Nil,
          _ ++ _,
          _ ++ _,
          (fs, _) => fs
        )

      def defects: List[Throwable] =
        fold[List[Throwable]](
          _ => Nil,
          List(_),
          (l, r) => l ++ r,
          (l, r) => l ++ r,
          (ds, _) => ds
        )

      def untraced: Cause[E] =
        fold[Cause[E]](
          e => Cause.fail(e),
          t => Cause.die(t),
          (l, r) => Cause.both(l, r),
          (l, r) => Cause.then(l, r),
          (cause, _) => cause
        )
    }

    object Cause {
      case class Fail[E](e: E) extends Cause[E]
      case class Die(throwable: Throwable) extends Cause[Nothing]
      case class Both[E](left: Cause[E], right: Cause[E]) extends Cause[E]
      case class Then[E](left: Cause[E], right: Cause[E]) extends Cause[E]
      case class Traced[E](cause: Cause[E], trace: Trace) extends Cause[E]

      def fail[E](error: E): Cause[E] =
        Fail(error)

      def die[E](throwable: Throwable): Cause[E] =
        Die(throwable)

      def both[E](left: Cause[E], right: Cause[E]): Cause[E] =
        Both(left, right)

      def then[E](left: Cause[E], right: Cause[E]): Cause[E] =
        Then(left, right)

      def traced[E](cause: Cause[E], trace: Trace): Cause[E] =
        Traced(cause, trace)
    }
  }

  type Trace = String

  object Advanced {
    import Cause.CauseCase
    import Cause._

    // Cause Case Class vs CauseCase Case Class
    final case class Cause[+E](caseValue: CauseCase[E, Cause[E]]) {

      // CATA : "down" (break, break, break it down)
      def fold[Z](f: CauseCase[E, Z] => Z): Z =
        f(caseValue.map(_.fold(f)))
      // caseValue match {
      //   case FailCase(e) =>
      //     f(FailCase(e))
      //   case DieCase(throwable) =>
      //     f(DieCase(throwable))
      //   case BothCase(left, right) =>
      //     f(BothCase(left.fold(f), right.fold(f)))
      //   case ThenCase(left, right) =>
      //     f(ThenCase(left.fold(f), right.fold(f)))
      //   case TracedCase(cause, trace) =>
      //     f(TracedCase(cause.fold(f), trace))
      // }
      def failures: List[E] =
        fold[List[E]] {
          case FailCase(e)           => List(e)
          case DieCase(_)            => Nil
          case BothCase(left, right) => left ++ right
          case ThenCase(left, right) => left ++ right
          case TracedCase(cause, _)  => cause
        }

      def defects: List[Throwable] =
        fold[List[Throwable]] {
          case FailCase(_)           => Nil
          case DieCase(t)            => List(t)
          case BothCase(left, right) => left ++ right
          case ThenCase(left, right) => left ++ right
          case TracedCase(cause, _)  => cause
        }

      def transform[E1](
          f: CauseCase[E, Cause[E1]] => CauseCase[E1, Cause[E1]]
      ): Cause[E1] =
        caseValue match {
          case FailCase(e)        => Cause(f(FailCase(e)))
          case DieCase(throwable) => Cause(f(DieCase(throwable)))
          case BothCase(left, right) =>
            Cause(f(BothCase(left.transform(f), right.transform(f))))
          case ThenCase(left, right) =>
            Cause(f(ThenCase(left.transform(f), right.transform(f))))
          case TracedCase(cause, trace) =>
            Cause(f(TracedCase(cause.transform(f), trace)))
        }

      def orDie(implicit ev: E <:< Throwable): Cause[Nothing] =
        transform[Nothing] {
          case FailCase(error)      => DieCase(ev(error))
          case c @ DieCase(_)       => c
          case c @ BothCase(_, _)   => c
          case c @ ThenCase(_, _)   => c
          case c @ TracedCase(_, _) => c
        }

      // def transformSome[E1](
      //     pf: PartialFunction[CauseCase[E, Cause[E1]], CauseCase[E1, Cause[E1]]]
      // ): Cause[E1] =
      //   transform[E1] {
      //     case pf(result) => result
      //     case other      => other
      //   }

      def untraced: Cause[E] =
        transform[E] {
          case TracedCase(cause, trace) => cause.caseValue
          case other                    => other
        }
    }

    object Cause {
      sealed trait CauseCase[+E, +Self] { self =>
        def map[Self2](f: Self => Self2): CauseCase[E, Self2] =
          self match {
            case FailCase(e) =>
              FailCase(e)
            case DieCase(throwable) =>
              DieCase(throwable)
            case BothCase(left, right) =>
              BothCase(f(left), f(right))
            case ThenCase(left, right) =>
              ThenCase(f(left), f(right))
            case TracedCase(cause, trace) =>
              TracedCase(f(cause), trace)
          }
      }

      case class FailCase[E](e: E) extends CauseCase[E, Nothing]
      case class DieCase[Self](throwable: Throwable)
          extends CauseCase[Nothing, Nothing]
      case class BothCase[Self](left: Self, right: Self)
          extends CauseCase[Nothing, Self]
      case class ThenCase[Self](left: Self, right: Self)
          extends CauseCase[Nothing, Self]
      case class TracedCase[Self](cause: Self, trace: Trace)
          extends CauseCase[Nothing, Self]
    }

    def fail[E](error: E): Cause[E] =
      Cause(FailCase(error))

    def die[E](throwable: Throwable): Cause[E] =
      Cause(DieCase(throwable))

    def both[E](left: Cause[E], right: Cause[E]): Cause[E] =
      Cause(BothCase(left, right))

    def then[E](left: Cause[E], right: Cause[E]): Cause[E] =
      Cause(ThenCase(left, right))

    def traced[E](cause: Cause[E], trace: Trace): Cause[E] =
      Cause(TracedCase(cause, trace))

    val exampleCause =
      both(
        traced(fail("fail"), "COOL FAIL"),
        traced(die(new Exception("DIEEEEeeeeEEe!")), "COOL DIE")
      )

    val exampleCause2 =
      Cause(
        BothCase(
          Cause(
            FailCase("fail")
          ),
          Cause(
            DieCase(new Exception("DIEEEEeeeeEEe!"))
          )
        )
      )

    val exampleCause3 =
      Cause(
        BothCase(
          Cause(
            FailCase(new Exception("fail"))
          ),
          Cause(
            DieCase(new Exception("DIEEEEeeeeEEe!"))
          )
        )
      )

  }

  val exampleTree =
    Branch(
      Branch(
        Leaf(1),
        3,
        Leaf(2)
      ),
      5,
      Branch(
        Leaf(3),
        5,
        Leaf(4)
      )
    )

  println(exampleTree.leaves)
  println(exampleTree.sum)

  import Advanced._
  import Advanced.Cause._

  val untraced =
    exampleCause.transform[String] {
      case TracedCase(cause, trace) => cause.caseValue
      case other                    => other
    }

  println(exampleCause)
  println(exampleCause.untraced)
  println(exampleCause3.orDie)

  // Cause(BothCase(Cause(FailCase(fail)),Cause(DieCase(java.lang.Exception: DIEEEEeeeeEEe!))))
  // Cause(BothCase(Cause(DieCase(java.lang.Exception: fail)),Cause(DieCase(java.lang.Exception: DIEEEEeeeeEEe!))))
}
