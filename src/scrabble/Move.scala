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
  def validSpread: Boolean = {
    val startx = placedSorted(0)._1.x
    val endx = placedSorted(placedSorted.size - 1)._1.x
    val starty = placedSorted(0)._1.y
    val endy = placedSorted(placedSorted.size - 1)._1.y
    val horizontal = starty == endy
    val vertical = startx == endx

    if (!horizontal && !vertical) false else true

    def isLinear: (Boolean, Int, Int) = {

      placedSorted.tail.foldLeft(true, startx, starty) {
        case ((bl, lastx: Int, lasty: Int), (pos, let)) =>
          if (horizontal) {
            val isHorizontal = pos.y == lasty
            if (!isHorizontal) return (false, 0, 0)

            val comesAfter = pos.x == lastx + 1

            if (comesAfter) (true, pos.x, pos.y) else {
              // Loop from the current position to the previous position, making sure there are squares inbetween that the word is built from
              val range = List.range(lastx, pos.x)

              val emptiesBetween = range.find { x =>
                val curPos = Pos.posAt(x, pos.y).get
                board.squareAt(curPos).isEmpty
              }

              if (!emptiesBetween.isEmpty) (true, pos.x, pos.y) else return (false, pos.x, pos.y)

            }

          } else {
            val isVertical = pos.x == lastx
            if (!isVertical) return (false, 0, 0)

            val comesAfter = pos.y == lasty + 1

            if (comesAfter) (true, pos.x, pos.y) else {
              // Loop from the current position to the previous position, making sure there are squares inbetween
              val range = List.range(lasty, pos.y)

              val emptiesBetween = range.find { y =>
                val curPos = Pos.posAt(pos.x, y).get
                board.squareAt(curPos).isEmpty
              }

              if (!emptiesBetween.isEmpty) (true, pos.x, pos.y) else return (false, pos.x, pos.y)
            }
          }
      }

    }

    isLinear._1
  }

}

object Main {
  def main(args: Array[String]) {
    val game = Game.init(List("jim", "joe"), Dictionary.load("C:\\workspace\\Scala\\scalascrabble\\src\\Dict\\en.txt"), LetterBag.init)

    val board = Board.init

    val newBrd = board.squares + (Pos.posAt(1, 3).get -> NormalSquare(Some(Letter('a', 1))))
    val testBoard = Board(newBrd)

    val placed = List(
      Pos.posAt(1, 1).get -> Letter('a', '1'),
      Pos.posAt(1, 4).get -> Letter('a', '1'),
      Pos.posAt(1, 2).get -> Letter('a', '1'))

    val blanks = List()

    val move = Move(Game(game.players, testBoard, game.playersMove, game.bag), placed, blanks)
    println(move.placedSorted)

    println(move.validSpread)
  }
}


