package scrabble

import scala.collection.parallel.Foreach

class Tile (letter: Char, value: Int) {

  
  
}

object Tile {
  //@TODO: Add distributions for each letter to each tuple
	val onePoints = List('E','A','I','O','N','R','T','L','S','U').map(ch => (ch,1))
	val twoPoints = List('D','G').map(ch => (ch,2))
	val threePoints = List('B','C','M','P').map(ch => (ch,3))
	val fourPoints = List('F','H','V','W','Y').map(ch => (ch,4))
	val fivePoints = List('K').map(ch => (ch,5))
	val eightPoints = List('J','X').map(ch => (ch,8))
	val tenPoints = List('Q','Z').map(ch => (ch,10))
	
	val allValues = onePoints:::twoPoints:::threePoints:::fourPoints:::fivePoints:::eightPoints:::tenPoints
	
	
}



