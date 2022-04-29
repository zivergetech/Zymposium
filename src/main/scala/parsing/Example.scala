package parsing

import zio.Chunk
import zio.parser.Syntax
// INVERTIBLE PARSING LIBRARY
// Syntax
// Parser and Pretty Printer at the same time!!!
// Parser[+A] = String => Option[A]
// Parser[Json] = String => Some(Json.Obj(Map(...)))
// Printer[-A] = (A) => String
// A => String => Some(A)
// String => Some(A) => String

final case class ChessCoord(row: Char, col: Int)
final case class ChessMove(from: ChessCoord, to: ChessCoord)
final case class ChessGame(moves: List[ChessMove])

object ExampleParser extends App {
  val example = ChessGame(
    List(
      ChessMove(ChessCoord('E', 8), ChessCoord('A', 1)),
      ChessMove(ChessCoord('E', 2), ChessCoord('B', 8)),
      ChessMove(ChessCoord('C', 7), ChessCoord('A', 2))
    )
  )

  val string =
    """
E8 -> A1
E2 -> B8
C7 -> A2
      """.trim

  val intSyntax =
    Syntax.digit.transform(
      _.toString.toInt,
      (_: Int).toString.head
    )
  //  final case class ChessCoord(row: Char, col: Int)
  // E8 -> CheesCoord('E', 8)
  // A1 -> ChessCoord('A', 1)
  val chessCoordSyntax: Syntax[String, Char, Char, ChessCoord, ChessCoord] =
    (Syntax.charIn("ABCDEFGH") ~ intSyntax).transform(
      { case (char, int) => ChessCoord(char, int) },
      { case ChessCoord(char, int) => (char, int) }
    )

//  final case class ChessMove(from: ChessCoord, to: ChessCoord)

  val spaces = Syntax.whitespace.unit(' ')

  // E8 -> B7 = ChessMove(ChessCoord('E', 8), ChessCoord('B', 7))
  // A1 -> B8 = ChessMove(ChessCoord('A', 1), ChessCoord('B', 8))
  val chessMoveSyntax: Syntax[String, Char, Char, ChessMove, ChessMove] =
    (chessCoordSyntax ~ spaces ~ Syntax.string("->", ()) ~ spaces ~ chessCoordSyntax)
      .transform(
        { case (from, to) => ChessMove(from, to) },
        { case ChessMove(from, to) => (from, to) }
      )

  // final case class ChessGame(moves: List[ChessMove])
  // new line separated chess moves
  val chessGameSyntax: Syntax[String, Char, Char, ChessGame, ChessGame] =
    chessMoveSyntax
      .repeatWithSep(Syntax.whitespace.unit('\n'))
      .transform(
        moves => ChessGame(moves.toList),
        { case ChessGame(moves) => Chunk.fromIterable(moves) }
      )

//  val result = chessCoordSyntax.printString(ChessCoord('A', 3))
//  println(result.toOption.get)
//  val parsed = chessCoordSyntax.parseString(result.toOption.get)
//  println(parsed)
//
  val moveResult = chessMoveSyntax.printString(ChessMove(ChessCoord('A', 1), ChessCoord('B', 8)))
  println(moveResult.toOption.get)
  val moveParsed = chessMoveSyntax.parseString("A1 -> B8")
  println(moveParsed)

  val gameResult = chessGameSyntax.printString(example)
  println(gameResult.toOption.get)
  val gameParsed = chessGameSyntax.parseString(gameResult.toOption.get)
  println(gameParsed)
}
