package freedom

import zio.test._

object IsomorphismSpec extends ZIOSpecDefault {

  def spec =
    suite("Isomorphism")(
      test("(A, B) <=> (B, A)") {
        check(Gen.int, Gen.string) { (i, s) =>
          val iso = Isomorphism[(Int, String), (String, Int)]

          val ab  = (i, s)
          val ab2 = iso.from(iso.to(ab))

          val ba  = (s, i)
          val ba2 = iso.to(iso.from(ba))

          assert(List(100, 200))(Assertion.hasAt(100)(Assertion.equalTo(10)))
        }
      },
      test("Option[A] <=> Either[Unit, A]") {
        check(Gen.int) { i =>
          val iso = Isomorphism[Option[Int], Either[Unit, Int]]

          val oi  = Some(i)
          val oi2 = iso.from(iso.to(oi))

          val ei  = Right(i)
          val ei2 = iso.to(iso.from(ei))

          assertTrue(oi2 == oi && ei2 == ei)
        }
      }
    )
}
