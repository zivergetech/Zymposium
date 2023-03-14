import zio._
import zio.openai._
import zio.openai.model._
import zio.openai.model.CreateCompletionRequest.Prompt

object Example extends ZIOAppDefault {

  lazy val loop: ZIO[Completions, Any, Unit] =
    for {
      input <- Console.readLine(">")
      _     <- if (input == "exit") ZIO.fail(()) else ZIO.unit
      result <- Completions.createCompletion(
                  model = "text-davinci-003",
                  prompt = Prompt.String(input),
                  temperature = Temperature(0.6)
                )
      _ <- Console.printLine(result.choices.flatMap(_.text.toOption).mkString(", "))
      _ <- loop
    } yield ()

  val run =
    loop.provide(Completions.default)
}