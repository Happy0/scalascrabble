package scrabble

case class Move(game: Game, placed: List[(Pos, Letter)], blanks: List[(Pos, Char)]) {

  /** Returns the updated game if the move is a valid scrabble move, otherwise returns a String with an explanation of why the move is invalid */
  //def updatedGame: Either[String, Game] = {}

  private val playerHasLetters: Boolean =
    {
      val doesntHave = placed.find { case (Pos(x, y, gr), let) => !game.currentPlayer.letters.contains(let) }
      doesntHave match {
        case None => true
        case Some(x) => false
      }

    }

  val board = game.board

  val placedSorted = placed.sortBy { case (pos: Pos, let: Letter) => (pos.x, pos.y) }

  //private val hasRepeats

  // Returns true if the letter are placed in a legal distribution (linear or horizontal) within the board range, and there are no already occupied squares
  def validSpread: Either[Boolean, String] = {
    val amountPlaced = placedSorted.size

    val startx = placedSorted(0)._1.x
    val endx = placedSorted(amountPlaced - 1)._1.x
    val starty = placedSorted(0)._1.y
    val endy = placedSorted(amountPlaced - 1)._1.y
    val horizontal = starty == endy
    val vertical = startx == endx

    if (!horizontal && !vertical) false else true

    def isLinear: (Boolean, Int, Int) = {

      placedSorted.tail.foldLeft(true, startx, starty) {
        case ((bl, lastx: Int, lasty: Int), (pos, let)) =>
          val isLinear = if (horizontal) pos.y == lasty else pos.x == lastx
          if (!isLinear) return (false, pos.x, pos.y)
          val comesAfter = if (horizontal) pos.x == lastx + 1 else pos.y == lasty + 1

          if (comesAfter) (true, pos.x, pos.y) else {
            // Loop from the current position to the previous position, making sure there are squares inbetween that the word is built from
            val range = if (horizontal) List.range(lastx + 1, pos.x) else List.range(lasty + 1, pos.y)

            val emptiesBetween = range.find { nxt =>
              val curPos = if (horizontal) Pos.posAt(nxt, pos.y).get else Pos.posAt(pos.x, nxt).get
              board.squareAt(curPos).isEmpty
            }
            println(emptiesBetween)

            if (!emptiesBetween.isDefined) (true, pos.x, pos.y) else return (false, pos.x, pos.y)

          }

      }

    }

    val (bl, x, y) = isLinear

    if (bl) return Left(true) else return Right("Letter placed at " + Pos.posAt(x, y).get.toString + " is an illegal move")
  }

}

object Main {
  def main(args: Array[String]) {
    val game = Game.init(List("jim", "joe"), Dictionary.load("C:\\workspace\\Scala\\scalascrabble\\src\\Dict\\en.txt"), LetterBag.init)

    val board = Board.init

    val newBrd = board.squares + (Pos.posAt(3, 1).get -> NormalSquare(Some(Letter('a', 1))))
    val testBoard = Board(newBrd)
    
    println(testBoard)

    val placed = List(
      Pos.posAt(4, 1).get -> Letter('A', 1),
      Pos.posAt(2, 1).get -> Letter('A', 1),
      Pos.posAt(1, 1).get -> Letter('A', 1))

    val blanks = List()

    val move = Move(Game(game.players, testBoard, game.playersMove, game.bag), placed, blanks)
    println(move.placedSorted)

    println(move.validSpread)
  }
}


