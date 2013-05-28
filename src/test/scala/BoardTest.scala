package scrabble
import org.specs2.matcher.ContainExactlyOnceMatcher

class BoardTest extends ScrabbleTest {

  val board = Board.init

  /** Place tiles on the board at the specified positions */
  def placeSquares(placed: List[(Pos, Tile)]): Board = placed.foldLeft(board) {
    case (b, placed) =>
      b.placeLetter(placed._1, placed._2)
  }

  "a board" should {
    
    
    "have 225 squares" in {
      board.squares must have size (225)
    }

    "place 1 tile in" in {
      board.placeLetter(Pos.posAt(3, 3).get, Letter('a', 1)
          ).squares.toTraversable must contain( (tup: (Pos, Square)) => tup._2.tile mustNotEqual None ).exactlyOnce
          
    }

    
    
  }

}