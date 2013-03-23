package scrabble;

case class Board (
    squares: Map[Pos, Square]
    
	){

  
}

object Board {
  import Pos._
  
  def apply(squares: Traversable[(Pos, Square)]): Board = Board(squares.toMap)
  
  def init : Board =
  {
    // Return a list of all the bonus squares. The rest of the board are then 'normal squares'
    val bonusSquares : Map[(Int,Int), Square] =
    {
      val min:Int = 1
      val max:Int = 15
      
      /* Quarter the board, then find the values from the rest of the board using the symmetry. I hope I can maths ;x..*/
      val leftQuarter : List[((Int,Int), Square)] =
      List( 
          (1,1) -> TripleWordSquare(None),
          (4,1) -> DoubleLetterSquare(None),
          (8,1) -> TripleWordSquare(None),
          (2,2) -> DoubleWordSquare(None),
          (6,2) -> TripleLetterSquare(None),
          (3,3) -> DoubleWordSquare(None),
          (7,3) -> DoubleLetterSquare(None),
          (1,4) -> DoubleLetterSquare(None),
          (4,4) -> DoubleWordSquare(None),
          (8,4) -> DoubleLetterSquare(None),
          (5,5) -> DoubleWordSquare(None),
          (2,6) -> TripleLetterSquare(None),
          (6,6) -> TripleLetterSquare(None),
          (3,7) -> DoubleLetterSquare(None),
          (7,7) -> DoubleLetterSquare(None),
          (1,8) -> TripleWordSquare(None),
          (4,8) -> DoubleLetterSquare(None)

          )
       val rightQuarter: List[((Int,Int), Square)] = leftQuarter.map{ case ((x,y), square) => ((max + 1 -x, y), square) }
       val bottomLeftQuarter: List[((Int,Int), Square)] = leftQuarter.map{ case ((x,y), square) => ((x, max + 1 - y), square) }
       val bottomRightQuarter: List[((Int,Int), Square)] = bottomLeftQuarter.map{ case ((x,y), square) => ((max + 1 -x, y), square) }
       
       // Return all the special squares
       (leftQuarter ::: rightQuarter ::: bottomLeftQuarter ::: bottomRightQuarter).toMap
    } 
    
    // Construct the board. Anything that is not a bonus square is a NormalSquare
    val all = Pos.all    
    val list : List[(Pos,Square)] = for {
        a <- all
        (x,y) = a
        special = bonusSquares.get(x, y)
        
        square:Square = special match {
          case None => NormalSquare(None)
          case Some(x) => x
        }
 
    } yield Pos.posAt(x:Int,y:Int).get -> square 

    Board(list.toMap)
  }
  
}

