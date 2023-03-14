import zio.nio.file.{ Files, Path }
import zio.{ Chunk, Console, ZIO, ZIOAppDefault }
import zio.openai.Images
import zio.openai.model.{ N, ResponseFormat, Size }

import java.util.Base64

/** Based on https://beta.openai.com/docs/api-reference/images
  */
object Image extends ZIOAppDefault {

  def createImageFromPrompt(string: String, n: Int) =
    for {
      response <- Images.createImage(
                    string,
                    n = N(2),
                    size = Size.`1024x1024`,
                    responseFormat = ResponseFormat.B64_json
                  )
      _        <- ZIO.foreachDiscard(response.data.zipWithIndex) { case (data, idx) =>
                    val imageData = Base64.getDecoder.decode(data.b64Json.getOrElse(""))
                    val path = Path(s"result-image-${n + idx}.png")

                    Files.writeBytes(path, Chunk.fromArray(imageData)) *> Console.printLine(
                      s"Image written to $path"
                    )
                  }
    } yield ()

  def loop(inputs: List[String]): ZIO[Images, Any, Unit] =
    ZIO.foreachDiscard(inputs.zipWithIndex) { case (input, index) =>
      createImageFromPrompt(input, index * 2)  
    }
  

  def run =
    loop(
      List(
        "A happy cat",
        "An angry cat"
      )
    ).provide(Images.default)
}