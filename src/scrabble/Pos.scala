package scrabble

sealed case class Pos private (x:Int, y: Int, gridCordinates: String) {
  
	import Pos.posAt
	
	// Inspired by ScalaChess. A convenient way to access neighbouring squares.
	lazy val up: Option[Pos] = posAt(x, y + 1)
	lazy val down: Option[Pos] = posAt(x, y -1)
	lazy val left: Option[Pos] = posAt(x - 1, y)
	lazy val right: Option[Pos] = posAt(x + 1, y)
	lazy val upLeft: Option[Pos] = up flatMap (_ left)
    lazy val upRight: Option[Pos] = up flatMap (_ right)
    lazy val downLeft: Option[Pos] = down flatMap (_ left)
    lazy val downRight: Option[Pos] = down flatMap (_ right)
 
  
}

object Pos {
	  
  /** Returns the position object at location (x,y) if it exists, else returns None (if it is out of the bounds of the board) */
  def posAt(x:Int, y:Int) : Option[Pos] = allPositions get (x,y)
  
  val all : List[(Int, Int)] = for {i <- List.range(1,16); j <- List.range(1,16)} yield i -> j
  
  lazy val allPositions : Map [(Int, Int), Pos] = 
  {
    
    // Letters mapped to columns, for displaying the move log
    val gridCoords = (List.range(1, 16) zip List.range('a','p')).toMap
    
    // Create the position objects and map them to their location tuple
    val x = for {
        a <- all 	      
        (x,y) = a
        letter = gridCoords.get(x).get 
        pos = Pos(x,y, letter.toString() + y)
    } yield (x,y) -> pos

     x.toMap
  }
  
  def main(args:Array[String]) {
    println(allPositions)
    println(allPositions.size)
  }

}

	

