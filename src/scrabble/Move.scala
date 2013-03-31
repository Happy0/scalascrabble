package scrabble

// Think about how to deal with blank letters. Seperate method?

class Move(board: Board, player: Player,  bag: LetterBag, dict: Dictionary, placed: List[(Pos, Letter)], blanks: List[Char])
{
  
  /** Returns the updated board if the move is a valid scrabble move, otherwise returns false */
  def updatedBoard : Option[Board] = {
    
    
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


