package scrabble
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

import scala.util.{ Try, Success, Failure }

trait ScrabbleTest extends Specification {
  val enDict = Dictionary.load("Dict/en.txt")
  
  val board = Board.init

  val letterBag = LetterBag.init

  def pos(x: Int, y: Int) = Pos.posAt(x, y).get

  val game = Game.make(List("jim", "joe"), enDict, LetterBag.init).get
  
  def letterFor(c: Char) = letterBag.tileSet.get(c.toUpper).get
  
  def toLetters(str: String) = str.toUpperCase.toList.map(c => letterFor(c))
  
  val predictableLetterBag = LetterBag.fromLetters(
      "LISVURDIGQAWLOEIYURADYEICBLEDHMSIXNFERAIWOANETGAELGFIUT_TJHAI_BDONENOECTRIEEREKOAZPVETONSASURAPMNOTO",
          LetterBag.init.tileSet).get
          
  val predictableLetterbagGame = Game.make(List("jim", "joe"), enDict, predictableLetterBag).get
          
          

  // Helper method to place a spread of letters on the board
  def toPlace(word: String, horizontal: Boolean, from: Pos): List[(Pos, Tile)] = {
    val positions = if (!horizontal) (from.y to from.y + word.size).map(c => pos(from.x,c))
    else (from.x to from.x + word.size).map(c => pos(c, from.y))
    
    positions zip toLetters(word) toList
  }

  val crossedWords: Board = {
    val horPlacements = toPlace("history", true, pos(3,5))
    val downPlacements = toPlace("scores", false, pos(7,3))
    
    val b = placeSquares(board, horPlacements)
    val newb = placeSquares(b, downPlacements)

    newb
  }

  /** Place tiles on the board at the specified positions */
  def placeSquares(board: Board, placed: List[(Pos, Tile)]): Board = placed.foldLeft(board) {
    case (b, placed) =>
      b.placeLetter(placed._1, placed._2)
  }

  /** A game which is in progress - or is 'constructed' to a certain scenario for the purposes of testing */
  implicit def playedGame(game: Game) = new {

    /** Place words on the board*/
    def playBoardMoves(placed: List[(Pos, Tile)]*): Try[Game] = ???

  }

}