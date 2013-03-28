package scrabble

/** tiles: The current tiles in the bag */
class LetterBag (tiles: List[Tile]) {
  
  

}

object LetterBag {
  
  def apply(tiles: List[Tile]) : LetterBag = LetterBag(tiles)
  
  def init: LetterBag = {
    
  
	val blankPoints = List(('_',0,2))
	val onePoints = List('E','A','I','O','N','R','T','L','S','U').map(ch => (ch,1))  //@TODO: Add distributions for each letter to each tuple
	
	val twoPoints = List(('D',2,4),('G',2,3))
	val threePoints = List('B','C','M','P').map(ch => (ch,3,2))
	val fourPoints = List('F','H','V','W','Y').map(ch => (ch,4,2))
	val fivePoints = List('K').map(ch => (ch,5,1))
	val eightPoints = List('J','X').map(ch => (ch,8,1))
	val tenPoints = List('Q','Z').map(ch => (ch,10,10))
	
	val allValues = blankPoints ::: onePoints:::twoPoints:::threePoints:::fourPoints:::fivePoints:::eightPoints:::tenPoints
  }
  
}