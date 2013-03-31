package scrabble

// Think about how to deal with blank letters. Seperate method?

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

    // Look for an element in the move list that breaks the horizontal / vertical distribution
    val find = placed.find { case (Pos(x, y, ch), let) => if (horizontal) (x != placed(0)._1.x) else if (vertical) (y != placed(0)._1.y) else false }

    find match {
      case None => true
      case Some(x) => false
    }

  }

  /*
  private val updatedPlayer: Player = 
  
  private val updatedBag = 
    
  private val updatedBoard = 
   
  */

}


