package scrabble

class BoardTest extends ScrabbleTest {

  val oneLetterPlaced = board.placeLetter(Pos.posAt(3, 3).get, Letter('a', 1))

  val normal = NormalSquare(None)
  val tripleWord = TripleWordSquare(None)
  val doubleLetter = DoubleLetterSquare(None)
  val doubleWord = DoubleWordSquare(None)
  val tripleLetter = TripleLetterSquare(None)

  "a board" should {

    "find letters above a position" in {
      crossedWords.LettersAbove(pos(7, 5)).map(tup => tup._2.letter).mkString must beEqualTo("RES")
    }

    "find letters below a position" in {
      crossedWords.LettersBelow(pos(7, 5)).map(tup => tup._2.letter).mkString must beEqualTo("SC")
    }

    "find letters left of a position" in {
      crossedWords.LettersLeft(pos(7, 5)).map(tup => tup._2.letter).mkString must beEqualTo("HIST")
    }

    "find letters right of a position" in {
      crossedWords.LettersRight(pos(7, 5)).map(tup => tup._2.letter).mkString must beEqualTo("RY")
    }

    /* Tedious, but important test to make sure all the special squares are positioned correctly */
    "should position special squares correctly" in {
      board.squares must havePairs(
        pos(1, 1) -> tripleWord,
        pos(4, 1) -> doubleLetter,
        pos(8, 1) -> tripleWord,
        pos(12, 1) -> doubleLetter,
        pos(15, 1) -> tripleWord,

        pos(2, 2) -> doubleWord,
        pos(6, 2) -> tripleLetter,
        pos(10, 2) -> tripleLetter,
        pos(14, 2) -> doubleWord,

        pos(3, 3) -> doubleWord,
        pos(7, 3) -> doubleLetter,
        pos(9, 3) -> doubleLetter,
        pos(13, 3) -> doubleWord,

        pos(1, 4) -> doubleLetter,
        pos(4, 4) -> doubleWord,
        pos(8, 4) -> doubleLetter,
        pos(12, 4) -> doubleWord,
        pos(15, 4) -> doubleLetter,

        pos(5, 5) -> doubleWord,
        pos(11, 5) -> doubleWord,

        pos(2, 6) -> tripleLetter,
        pos(6, 6) -> tripleLetter,
        pos(10, 6) -> tripleLetter,
        pos(14, 6) -> tripleLetter,

        pos(3, 7) -> doubleLetter,
        pos(7, 7) -> doubleLetter,
        pos(9, 7) -> doubleLetter,
        pos(13, 7) -> doubleLetter,

        pos(1, 8) -> tripleWord,
        pos(4, 8) -> doubleLetter,
        pos(8, 8) -> doubleWord,
        pos(12, 8) -> doubleLetter,
        pos(15, 8) -> tripleWord,

        pos(3, 9) -> doubleLetter,
        pos(7, 9) -> doubleLetter,
        pos(9, 9) -> doubleLetter,
        pos(13, 9) -> doubleLetter,

        pos(2, 10) -> tripleLetter,
        pos(6, 10) -> tripleLetter,
        pos(10, 10) -> tripleLetter,
        pos(14, 10) -> tripleLetter,

        pos(5, 11) -> doubleWord,
        pos(11, 11) -> doubleWord,

        pos(1, 12) -> doubleLetter,
        pos(4, 12) -> doubleWord,
        pos(8, 12) -> doubleLetter,
        pos(12, 12) -> doubleWord,
        pos(15, 12) -> doubleLetter,

        pos(3, 13) -> doubleWord,
        pos(7, 13) -> doubleLetter,
        pos(9, 13) -> doubleLetter,
        pos(13, 13) -> doubleWord,

        pos(2, 14) -> doubleWord,
        pos(6, 14) -> tripleLetter,
        pos(10, 14) -> tripleLetter,
        pos(14, 14) -> doubleWord,

        pos(1, 15) -> tripleWord,
        pos(4, 15) -> doubleLetter,
        pos(8, 15) -> tripleWord,
        pos(12, 15) -> doubleLetter,
        pos(15, 15) -> tripleWord)
    }

    "have 61 special squares" in {
      board.squares.toTraversable filter (p => p._2 != normal) must have size 61
    }

    "have and 164 normal squares" in {
      board.squares.toTraversable filter (p => p._2 == normal) must have size 164
    }

    "have 225 squares" in {
      board.squares must have size 225
    }

    "place 1 tile" in {
      oneLetterPlaced.squares.toTraversable filter (p => !p._2.isEmpty) must have size 1
    }

    "place Tile in the correct position" in {
      oneLetterPlaced.squareAt(pos(3, 3)).tile must beEqualTo(Some(Letter('a', 1)))
    }

    "retrieve an occupied square" in {
      oneLetterPlaced.squareAt(pos(3, 3)).tile must beEqualTo(Some(Letter('a', 1)))
    }

  }

}