package scrabble;

case class Board (
    squares: Map[Pos, Square]
    
	){

  
}

object Board {
  import Pos._
  
  def apply(squares: Traversable[(Pos, Square)]): Board = Board(squares.toMap)
  
 // def init : Board = {
    
  //}
  
}

