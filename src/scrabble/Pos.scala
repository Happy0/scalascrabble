package scrabble

sealed case class Pos private (x:Int, y: Int, gridCordinates: String) {
  
	import Pos.posAt
	
	// Inspired by ScalaChess
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
	  
  def posAt(x:Int, y:Int) : Option[Pos] = allPositions get (x,y)
  
  // Wrong: work out how to do it correctly
  //val all : List[(Int, Int)] = List.range(1,16) zip List.range(1, 16)
  
  val all : List[(Int, Int)] = for {i <- List.range(1,16); j <- List.range(1,16)} yield i -> j
  
  lazy val allPositions : Map [(Int, Int), Pos] = 
  {
    
    val gridCoords = (List.range(1, 16) zip List.range('a','p')).toMap
    
    val x = for {
        up <- all 	      
        (x,y) = up
        pos = Pos(x,y, gridCoords.get(x).toString() + y)
    } yield (x,y) -> pos

     x.toMap
  }
  
  def main(args:Array[String]) {
    println(allPositions)
    println(allPositions.size)
  }

}

	

