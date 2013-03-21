sealed case class Pos private (x:Int, y: Int, gridCordinates: String){
  
	import Pos.posAt
	
	lazy val up: Option[Pos] = posAt(x, y + 1)
	lazy val down: Option[Pos] = posAt(x, y -1)
	lazy val left: Option[Pos] = posAt(x - 1, y)
	lazy val right: Option[Pos] = posAt(x + 1, y)
 
  
}

	object Pos {
	  
	  def posAt(x:Int, y:Int) : Option[Pos] = allPositions get (x,y)
	  
	  lazy val allPositions : Map [(Int, Int), Pos] = 
	  {
	    val all : List[(Int, Int)] = List.range(1,16) zip List.range(1, 16)
	    val gridCoords = (List.range(1, 16) zip List.range('a','h')).toMap
	    
	    val x = for {
	        up <- all 	      
	        (x,y) = up
	        pos = Pos(x,y, gridCoords.get(x).toString() + x)
	    } yield (x,y) -> pos

	     x.toMap
	  }
}