package api

// interpret(Syntax[A]): Semantics[A]
import api.Backend.Handler
import zhttp.http.{HttpApp, Method, Request, URL}
import zio.{ZIO, Zippable}
import zio.json.internal.{RetractReader, Write}
import zio.json.{JsonCodec, JsonDecoder, JsonEncoder, JsonError}

import java.util.UUID

final case class API[Input, Output](
  method: Method,
  path: Route[Input],
  outputCodec: JsonCodec[Output]
) {
  def toHttp[R, E, Output2](f: Input => ZIO[R, E, Output2])(implicit codec: JsonCodec[Output2]): HttpApp[R, E] =
    Handler.make(this.copy(outputCodec = codec))(f).toHttp

  def parse(request: Request): Option[Input] = {
    println(s"API.parse(${request.method}, ${request.url})")
    if (request.method == method)
      path.parse(request.path.toList)
    else
      None
  }

  def output[Output2](implicit codec: JsonCodec[Output2]): API[Input, Output2] =
    copy(outputCodec = codec)

}

object API {
  def get[Input](path: Route[Input]): API[Input, Unit] =
    API(Method.GET, path, unitJsonCodec)

  def post[Input](path: Route[Input]): API[Input, Unit] =
    API(Method.POST, path, unitJsonCodec)

  lazy val unitJsonCodec: JsonCodec[Unit] =
    JsonCodec(
      (a: Unit, indent: Option[Int], out: Write) => (),
      (trace: List[JsonError], in: RetractReader) => ()
    )
}

// concrete terms
// literal:   "users" "home" "friends"
// extractor: Int Uuid String
// /users/home -> Zip
// /users/[uuid]
// /users/[int]/friends
// /users/[string]/friends/[string]
// Leaf Nodes / Base Cases              -> Constructors
// Branches / Inductive/Recursive Cases -> Combinators
object Route {
  // List()
  // List("users", "home")
  // List("posts", "123")
  final case class Literal(string: String) extends Route[Unit] {
    override def parseImpl(segments: List[String]): Option[(List[String], Unit)] =
      if (segments.headOption.contains(string))
        Some(segments.tail -> ())
      else None
  }

  // List()
  // List("123", "home")
  // List("hello", "123")
  final case class Extractor[A](name: String, from: String => Option[A]) extends Route[A] {
    override def parseImpl(segments: List[String]): Option[(List[String], A)] =
      segments.headOption.flatMap(from).map(a => segments.tail -> a)
  }

  final case class ZipWith[A, B, C](left: Route[A], right: Route[B], f: (A, B) => C) extends Route[C] {
    override def parseImpl(segments: List[String]): Option[(List[String], C)] =
      for {
        (remaining, a) <- left.parseImpl(segments)
        (remaining, b) <- right.parseImpl(remaining)
      } yield remaining -> f(a, b)
  }

  implicit def literal(value: String): Route[Unit]                    = Literal(value)
  def extractor[A](name: String, from: String => Option[A]): Route[A] = Extractor(name, from)

  val uuid: Route[UUID] =
    Route.extractor[UUID]("uuid", string => scala.util.Try(UUID.fromString(string)).toOption)

  val int: Route[Int] =
    Route.extractor[Int]("int", _.toIntOption)

  val string: Route[String] =
    Route.extractor[String]("string", Some(_))

}

sealed trait Route[A] extends Product with Serializable { self =>

  def /[B](that: Route[B])(implicit zippable: Zippable[A, B]): Route[zippable.Out] =
    Route.ZipWith[A, B, zippable.Out](self, that, zippable.zip(_, _))

  def render: String =
    self.asInstanceOf[Route[_]] match {
      case Route.Literal(string) =>
        string
      case Route.Extractor(name, _) =>
        s"[$name]"
      case Route.ZipWith(left, right, _) =>
        s"${left.render}/${right.render}"
    }

  def parse(segments: List[String]): Option[A] =
    parseImpl(segments).map(_._2)

  def parseImpl(segments: List[String]): Option[(List[String], A)]
}

object PathExamples extends App {
  // /users/home
  val usersHome = Route.literal("users") / "home"
  println(usersHome.render)

  // /users/[uuid]
  val usersUuid =
    Route.literal("users") / Route.uuid

  println(usersUuid.render)

  // /users/[uuid]/friends/[string]
  val usersUuidFriendsString: Route[(Int, String, Int)] =
    Route.literal("users") / Route.int / "friends" / Route.string / Route.int

  val examplePath = List("users", "555", "friends", "adam")

  val request =
    Request.apply(method = Method.GET, url = URL(path = zhttp.http.Path(examplePath)))

  import Route._

  val getUsersFriends = API.get("users" / int / "friends" / string)
  println(getUsersFriends.parse(request))

}
