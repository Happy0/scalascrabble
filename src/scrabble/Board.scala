package scrabble;

case class Board (
    squares: Map[Pos, Square]
    
	){

  
}

object Board {
  import Pos._
  
  def apply(squares: Traversable[(Pos, Square)]): Board = Board(squares.toMap)
  
  // Oh dear god... I can't maths / symmetry
  def init : Board = {
    val all = Pos.all
    
    val bonusQuarter : List[((Int,Int), Square)] = {
      List( 
          (1,1) -> TripleWordSquare(None),
          (4,1) -> DoubleLetterSquare(None),
          (8,1) -> TripleWordSquare(None)
          
          
         
          )
                  
    } 
    
  }
  
}

