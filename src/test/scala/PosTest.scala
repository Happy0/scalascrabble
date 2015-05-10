package scrabble

class PosTest extends ScrabbleTest {

  "a position should" should {

    "be within the 15 x 15 boundary of the board" in {
      val check: List[(Int, Int)] = for { i <- List.range(-10, 30); j <- List.range(-10, 30) } yield i -> j

      check.foreach {
        case (x, y) =>
          if (x < 1 || x > 15 || y < 1 || y > 15) Pos.at(x, y) must beNone else Pos.at(x, y) must beSome

      }

    }

    "Correctly label the X axis with a letter" in {
      val gridLetters = (1 to 15) zip ('A' until 'P') toMap

      Pos.allPositionsMap foreach {
        case (_, Pos(x, y, coord)) =>
          val let = gridLetters get x
          let should beSome

          let foreach {
            let =>
              coord must be equalTo (let.toString + y)
          }

      }
    }
  }

}