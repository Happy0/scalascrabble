package scrabble

case class Move(game: Game, placed: List[(Pos, Letter)], blanks: List[Char]) {

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

  //private val hasRepeats

  // Returns true if the letter are placed in a legal distribution (linear or horizontal) within the board range, and there are no already occupied squares
  def validSpread: Boolean = {
    val startx = placed(0)._1.x
    val endx = placed(placed.size - 1)._1.x
    val starty = placed(0)._1.y
    val endy = placed(placed.size - 1)._1.y
    val horizontal = starty == endy
    val vertical = startx == endx

    if (!horizontal && !vertical) false else true

    def isLinear: (Boolean, Int, Int) = {
      placed.tail.foldLeft(true, startx, starty) {
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

    val placed = List(
      Pos.posAt(1, 1).get -> Letter('a', '1'),
      Pos.posAt(2, 1).get -> Letter('a', '1'),
      Pos.posAt(5, 1).get -> Letter('a', '1'))

    val blanks = List()

    val move = Move(game, placed, blanks)

    println(move.validSpread)
  }
}


