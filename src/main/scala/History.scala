package scrabble

import scala.util.{ Try, Success, Failure }
import scalaz.NonEmptyList

abstract class MoveSummary 

case class PlaceSummary(placed: NonEmptyList[PosTile], formedWords: FormedWords, score: Score) extends MoveSummary()

case class SkippedSummary() extends MoveSummary()

case class ExchangedSummary(given: List[Tile], newBag: String) extends MoveSummary()

case class History(startGame: Game, moveHistory: NonEmptyList[MoveSummary]) {

  def addToLog(summary: MoveSummary): History = copy(moveHistory = summary <:: moveHistory)

  def latestMove: MoveSummary = moveHistory.head

  // This seems inelegant. There must be another way... :o
  def latestPlace: Option[PlaceSummary] = moveHistory.list collectFirst {
    case PlaceSummary(placed, formedWords, score) => PlaceSummary(placed, formedWords, score)
  }

  def replayMove(game: Game, summary: MoveSummary): Try[Game] = {
    summary match {
      case PlaceSummary(placed, _, _) => PlaceLettersMove(game, placed).validate flatMap (_.makeMove)
      case SkippedSummary() => PassMove(game).makeMove
      case ExchangedSummary(given, newBag) =>
        ExchangeMove(game, given).makeMove flatMap {
          g =>
            LetterBag.fromLetters(newBag, g.bag.tileSet).toTry(Failure(LetterNotInTileSetClientError())) { replaceBag =>
              Success(game.copy(bag = replaceBag))
            }
        }

    }
  }
  
  def logInOrder = moveHistory.reverse

  def stepThrough: Iterator[Game] = {
    val it = moveHistory.reverse.tail.iterator.scanLeft(replayMove(startGame, moveHistory.head)) {
      case (tryGame, summary) => tryGame.flatMap(g => replayMove(g, summary))
    }

    it collect { case (Success(x)) => x }
  }

}