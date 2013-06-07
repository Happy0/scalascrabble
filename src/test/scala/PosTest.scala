package scrabble

class PosTest extends ScrabbleTest {

  "a position should" should {

    "be within the 15 x 15 boundary of the board" in {
      val check: List[(Int, Int)] = for { i <- List.range(-10, 30); j <- List.range(-10, 30) } yield i -> j

      check.foreach {
        case (x, y) =>
          if (x < 1 || x > 15 || y < 1 || y > 15) Pos.posAt(x, y) must beNone else Pos.posAt(x, y) must beSome

      }

    }
  }

}