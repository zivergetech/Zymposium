package freedom

// Cardinality
// Isomorphisms

final case class Isomorphism[A, B](
  to: A => B,
  from: B => A
)

object Isomorphism {
  def apply[A, B](implicit iso: Isomorphism[A, B]): Isomorphism[A, B] = iso

  implicit def iso[A, B]: Isomorphism[(A, B), (B, A)] =
    Isomorphism(
      _.swap,
      _.swap
    )

  implicit def isoOption[A]: Isomorphism[Option[A], Either[Unit, A]] =
    Isomorphism(
      {
        case Some(a) => Right(a)
        case None    => Left(())
      },
      {
        case Left(()) => None
        case Right(a) => Some(a)
      }
    )
}

// Folds
// Data => Suspended Interpretation
// Data <= Functions
object Folds {

  sealed trait List[+A] {
    def foldRight[B](z: => B)(op: (A, B) => B): B = ???
  }

  def identity[A](as: List[A]): List[A] =
    as.foldRight[List[A]](List.Empty)(List.Cons(_, _))

  object List {
    case object Empty                                extends List[Nothing]
    final case class Cons[A](head: A, tail: List[A]) extends List[A]
  }

  sealed trait Option[+A] extends Product with Serializable { self =>
    def fold[B](ifNone: => B)(ifSome: A => B): B =
      self match {
        case Option.None        => ifNone
        case Option.Some(value) => ifSome(value)
      }
  }

  trait ChurchOption[+A] {
    def apply[B](ifNone: => B)(ifSome: A => B): B

    def isEmpty: ChurchBoolean =
      this(ChurchBoolean.True)(_ => ChurchBoolean.False)

    def isDefined: ChurchBoolean =
      this(ChurchBoolean.False)(_ => ChurchBoolean.True)
  }

  object ChurchOption {
    val None =
      new ChurchOption[Nothing] {
        override def apply[B](ifNone: => B)(ifSome: Nothing => B): B =
          ifNone
      }

    def Some[A](value: A): ChurchOption[A] =
      new ChurchOption[A] {
        override def apply[B](ifNone: => B)(ifSome: A => B): B =
          ifSome(value)
      }
  }

  def identityOption[A](optionA: Option[A]): Option[A] =
    optionA.fold[Option[A]](Option.None)(Option.Some(_))

  object Option {
    case object None                   extends Option[Nothing]
    final case class Some[A](value: A) extends Option[A]
  }

  // type Coolean = (A, A) => A
  trait ChurchBoolean {
    def apply[A](ifTrue: => A, ifFalse: => A): A

    def &&(that: ChurchBoolean): ChurchBoolean =
      this(
        that(
          ChurchBoolean.True,
          ChurchBoolean.False
        ),
        ChurchBoolean.False
      )

    override def toString: String =
      this("true", "false")
//      Coolean.ifThenElse(this)("true", "false")
  }

  object ChurchBoolean {
    val True = new ChurchBoolean {
      override def apply[A](ifTrue: => A, ifFalse: => A): A = ifTrue
    }

    val False = new ChurchBoolean {
      override def apply[A](ifTrue: => A, ifFalse: => A): A = ifFalse
    }

    def ifThenElse[A](coolean: ChurchBoolean)(ifTrue: => A)(ifFalse: => A): A =
      coolean(ifTrue, ifFalse)
  }

  sealed trait Boolean { self =>
    def fold[B](ifTrue: => B)(ifFalse: => B): B =
      self match {
        case Boolean.True  => ifTrue
        case Boolean.False => ifFalse
      }

    override def toString: String =
      fold("true")("false")
  }

  object Boolean {
    case object True  extends Boolean
    case object False extends Boolean
  }

  def main(args: Array[String]): Unit = {
    println(ChurchNat.Four.toInt)
    println(ChurchNat.Two.toInt)
  }

  trait ChurchNat {
    def apply[B](ifZero: => B)(ifSucc: B => B): B

    def toInt: Int =
      apply(0)(_ + 1)
  }

  object ChurchNat {
    val Zero =
      new ChurchNat {
        override def apply[B](ifZero: => B)(ifSucc: B => B): B = ifZero
      }

    def Succ(nat: ChurchNat) =
      new ChurchNat {
        override def apply[B](ifZero: => B)(ifSucc: B => B): B =
          ifSucc(nat(ifZero)(ifSucc))
      }

    val Two  = Succ(Succ(Zero))
    val Four = Succ(Succ(Succ(Succ(Zero))))
  }

  // 0 1 2 3 4 5 6
  sealed trait Nat extends Product with Serializable { self =>
    def fold[B](ifZero: => B)(ifSucc: B => B): B =
      self match {
        case Nat.Zero      => ifZero
        case Nat.Succ(nat) => ifSucc(nat.fold(ifZero)(ifSucc))
      }

    def toInt: Int =
      fold(0)(_ + 1)
  }

  object Nat {
    // => B
    case object Zero extends Nat
    // B => B
    final case class Succ(nat: Nat) extends Nat

    val One   = Succ(Zero)
    val Two   = Succ(Succ(Zero))
    val Three = Succ(Succ(Succ(Zero)))
    val Four  = Succ(Succ(Succ(Succ(Zero))))
  }

}

// ZLayer zio-magic
// Service
// layerC: (A && B) => C
// layerB: D => B
// layerA: () => A
// layerD: () => D
// ZLayer.make[C](layerC, layerB, layerA, layerD, layerZ)
// (layerA ++ (layerD >>> layerB)) >>> layerC

// A =========> B => C
// A =========> B => C'

final case class Type(string: String)
final case class Expr(string: String) {
  def ++(that: Expr): Expr =
    Expr(s"($string ++ ${that.string})")

  def >>>(that: Expr): Expr =
    Expr(s"($string >>> ${that.string})")
}

object Expr {
  implicit val layerLike = new LayerLike[Expr] {
    override def ++(left: Expr, right: Expr): Expr = left ++ right

    override def >>>(left: Expr, right: Expr): Expr = left >>> right
  }
}

final case class Node[+A](inputs: List[Type], outputs: List[Type], expr: A)

// Data <=> Function
trait LayerLike[A] {
  def ++(left: A, right: A): A
  def >>>(left: A, right: A): A
}

sealed trait Layer[+A] extends Product with Serializable { self =>

  def fold[B](
    ifValue: A => B
  )(ifHorizontal: (B, B) => B)(ifVertical: (B, B) => B): B =
    self match {
      case Layer.Horizontal(left, right) =>
        ifHorizontal(
          left.fold(ifValue)(ifHorizontal)(ifVertical),
          right.fold(ifValue)(ifHorizontal)(ifVertical)
        )
      case Layer.Vertical(left, right) =>
        ifVertical(
          left.fold(ifValue)(ifHorizontal)(ifVertical),
          right.fold(ifValue)(ifHorizontal)(ifVertical)
        )
      case Layer.Value(value) => ifValue(value)
    }

  def ++[A1 >: A](that: Layer[A1]): Layer[A1] =
    Layer.Horizontal(this, that)

  def >>>[A1 >: A](that: Layer[A1]): Layer[A1] =
    Layer.Vertical(this, that)
}

object Layer {
  final case class Horizontal[A](left: Layer[A], right: Layer[A]) extends Layer[A]
  final case class Vertical[A](left: Layer[A], right: Layer[A])   extends Layer[A]
  final case class Value[A](value: A)                             extends Layer[A]
}

final case class Graph[A](nodes: List[Node[A]]) {
  def findNode(output: Type): Node[A] =
    nodes.find(_.outputs.contains(output)).get

  // List(Type("C"))
  // ==========>
  def build(target: List[Type]): Layer[A] = {
    val result: List[Layer[A]] = target.map { t =>
      val node: Node[A] = findNode(t)
      val dependencies: List[Layer[A]] =
        node.inputs.map(t => build(List(t)))
      if (dependencies.isEmpty) Layer.Value(node.expr)
      else dependencies.reduce(_ ++ _) >>> Layer.Value(node.expr)
    }
    result.reduce(_ ++ _)
  }

  // =============>
//  def unused(target: List[Type]): List[Node] = {
  //
//  }
}

object Examples {
  val layerCNode =
    Node(
      inputs = List(Type("A"), Type("B")),
      outputs = List(Type("C")),
      expr = Expr("layerC")
    )

  val layerBNode =
    Node(
      inputs = List(Type("D")),
      outputs = List(Type("B")),
      expr = Expr("layerB")
    )

  val layerANode =
    Node(inputs = List(), outputs = List(Type("A")), expr = Expr("layerA"))

  val layerDNode =
    Node(inputs = List(), outputs = List(Type("D")), expr = Expr("layerD"))

  val layerZNode =
    Node(
      inputs = List(Type("A"), Type("B")),
      outputs = List(Type("X")),
      expr = Expr("layerZ")
    )

  val graph =
    Graph(
      List(
        layerCNode,
        layerBNode,
        layerANode,
        layerDNode,
        layerZNode
      )
    )

  def main(args: Array[String]): Unit = {
    val layer: Layer[Expr] = graph.build(List(Type("C")))
    val expr               = layer.fold[Expr](identity)(_ ++ _)(_ >>> _)
    val used               = layer.fold[List[Expr]](a => List(a))(_ ++ _)(_ ++ _)
    val count              = used.length
    println(layer)
    println(expr.string)
    println(used)
  }
}
