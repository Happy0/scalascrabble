package scrabble
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification

import scala.util.{ Try, Success, Failure }

trait ScrabbleTest extends Specification {
  val board = Board.init
  
  def pos(x: Int, y: Int) = Pos.posAt(x, y).get

  val crossedWords: Board = {
    val historyPos = (3 until 3 + "history".length()) map (x => pos(x, 5)) toList
    val tiles = "history".toList.map(c => Letter(c, 1))

    // Needs fixed
    val scoresPos = (3 until 3 + "scores".length()) map (x => pos(7, x)) toList
    val downTiles = "scores".toList.map(c => Letter(c, 1))

    val horPlacements = historyPos zip tiles
    val downPlacements = scoresPos zip downTiles
    val b = placeSquares(board, horPlacements)
    val newb = placeSquares(b, downPlacements)
    println(newb)

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