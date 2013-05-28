package scrabble
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

import scala.util.{ Try, Success, Failure }


trait ScrabbleTest extends Specification {

  /** A game which is in progress - or is 'constructed' to a certain scenario for the purposes of testing */
  implicit def playedGame(game: Game) = new {
	  
    /** Place words on the board*/
    def playBoardMoves(placed: List[(Pos, Tile)] *) : Try[Game] = ???
    
 
  }

}