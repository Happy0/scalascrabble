package scrabble

class Move(game: Game, placed: List[(Pos, Letter)], blanks: List[Char]) {

  /** Returns the updated game if the move is a valid scrabble move, otherwise returns a String with an explanation of why the move is invalid */
  def updatedGame: Either[String, Game] = {

  }

  private val playerHasLetters: Boolean =
    {
      val doesntHave = placed.find { case (Pos(x, y, gr), let) => !game.currentPlayer.letters.contains(let) }
      doesntHave match {
        case None => true
        case Some(x) => false
      }
    }

  // Returns true if the letter are placed in a legal distribution (linear or horizontal)
  private def validSpread: Boolean = {
    val horizontal = placed(0)._1.x == placed(placed.size - 1)._1.x
    val vertical = placed(0)._1.y == placed(placed.size - 1)._1.y

    //@TODO: Make this tidier. Use a recursive function rather than using 'return' to jump out of the nested function?

    // Look for an element in the move list that breaks the horizontal / vertical distribution or is out of range of the board
    def isValid: (Boolean, Int, Int) = {
      placed.foldLeft(true, placed(0)._1.x, placed(0)._1.y) {
        case ((bool: Boolean, lastx: Int, lasty: Int), (Pos(x, y, ch), let)) =>

          val curPos = Pos.posAt(x, y)
          if (!curPos.isDefined) return (false, x, y) // If the position given isn't in range of the board, return false

          if (horizontal) {
            val hor = x == placed(0)._1.x

            lazy val pos = Pos.posAt(lastx + 1, y)
            lazy val defined = pos.isDefined
            lazy val occupied = defined && !game.board.squareAt(pos.get).isEmpty

            val correctY = (x == x + 1 || occupied)
            if (hor || occupied) (true, x, y) else return { (false, x, y) }

          } else if (vertical) {
            val ver = y == placed(0)._1.y
            lazy val pos = Pos.posAt(lastx + 1, y)
            lazy val defined = pos.isDefined
            lazy val occupied = defined && !game.board.squareAt(pos.get).isEmpty
            val correctY = (y == y + 1 || occupied)

            if (ver || occupied) (true, x, y) else return { (false, x, y) }
          } else {
            return { (false, x, y) }
          }
      }
    }

    return isValid._1
  }

  /*
  private val updatedPlayer: Player = 
  
  private val updatedBag = 
    
  private val updatedBoard = 
   
  */

}


