package scrabble

// Think about how to deal with blank letters. Seperate method?

class Move(game: Game, placed: List[(Pos, Letter)], blanks: List[Char])
{
  
  /** Returns the updated game if the move is a valid scrabble move, otherwise returns a String with an explanation of why the move is invalid */
  def updatedGame: Either[String, Game] = {
    
  }
  
  // Returns true if the letter are placed in a legal distribution (linear, and attached to an existing letter)
  def validSpread (placed: List[(Pos, Letter)]) : Boolean = {
    
  }
  
  /*
  val updatedPlayer: Player = 
  
  val updatedBag = 
    
  val updatedBoard = 
   
  */

}


