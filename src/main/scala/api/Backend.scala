package api

import zio._
import zio.json._

import java.util.UUID

object Backend extends ZIOAppDefault {
  import zhttp.http._
  import zhttp.service.Server

  final case class CreateUser(email: String) extends AnyVal
  object CreateUser {
    implicit val codec: JsonCodec[CreateUser] = DeriveJsonCodec.gen[CreateUser]
  }

//  trait ServerApi {
//    def allUsers: ZIO[Any, Nothing, List[User]]
//    def createUser(user: CreateUser): ZIO[Any, Nothing, User]
//    def getUser(userId: UserId): ZIO[Any, Nothing, Option[User]]
//  }

  // val getUserAPI: API[UUID, Option[User]] =
  //   API.get("users" / uuid).output[Option[User]]
  //   API.post("users" / uuid).output[Option[User]]
  //
  // Free Structures and Church Encodings
  // Data vs. Function
  // Data is only as good as its interpretation
  // Syntax => Semantics
  //
  // Syntax => Server
  // Server: Request => Response
  // RequestParser[Input]: Request => Option[Input]
  // API[Input, Output] -> f(input) -> output -> Response

  // interpret(Syntax[A]): Semantics[A]

  // api: API[A]
  // handler: Handler[Input, Output](api: API[A, Output], handler: A => Output)
  // Handler -> Server

  final case class Handler[R, E, Input, Output]( //
    api: API[Input, Output],
    handle: Input => ZIO[R, E, Output]
  ) {
    def toHttp: HttpApp[R, E] = {
      val matcher                                 = (api.parse _).unlift
      implicit val outputCodec: JsonCodec[Output] = api.outputCodec
      Http.collectZIO[Request] { //
        case matcher(input) =>
          handle(input).map { output =>
            Response.json(output.toJson)
          }
      }
    }
  }

  object Handler {
    def make[R, E, Input, Output](api: API[Input, Output])(
      handler: Input => ZIO[R, E, Output]
    ): Handler[R, E, Input, Output] =
      Handler(api, handler)
  }

  import Route._

  val userApp: HttpApp[Users, Throwable] =
    API.get("users" / Route.uuid).toHttp(userId => Users.get(UserId(userId)))

  val app: HttpApp[Users, Throwable] =
    Http.collectZIO[Request] {
      case Method.GET -> !! / "users" =>
        Users.all.map { users =>
          Response.json(users.toJson)
        }

      case req @ Method.POST -> !! / "users" =>
        for {
          body       <- req.bodyAsString
          createUser <- ZIO.from(body.fromJson[CreateUser]).mapError(new Error(_))
          user       <- Users.create(createUser.email)
        } yield Response.json(user.toJson)
    }

  val run =
    Server
      .start(8888, (userApp ++ app) @@ Middleware.debug ++ Http.notFound)
      .provide(Users.live)
}
