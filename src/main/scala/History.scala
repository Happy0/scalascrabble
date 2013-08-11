package scrabble

import scala.util.{ Try, Success, Failure }
import scalaz.NonEmptyList

sealed abstract class MoveSummary

case class PlaceSummary(placed: NonEmptyList[PosTile], formedWords: FormedWords, score: Score) extends MoveSummary()

case object SkippedSummary extends MoveSummary()

case class ExchangedSummary(given: List[Tile], newBag: String) extends MoveSummary()

case class History(startGame: Game, moveHistory: NonEmptyList[MoveSummary]) {

  def originalBag = {
    val sortedPlayers = startGame.players.toList.sortBy(x => x._1)

    sortedPlayers.foldRight(startGame.bag.lettersAsString) {
      case ((_, player), str) => player.letters.map(_.letter).mkString ++ str
    }
  }

  def addToLog(summary: MoveSummary): History = copy(moveHistory = summary <:: moveHistory)

  def latestMove: MoveSummary = moveHistory.head

  def latestPlace: Option[PlaceSummary] = moveHistory.list collectFirst { case p: PlaceSummary => p }

  def replayMove(game: Game, summary: MoveSummary): Try[Game] = {
    summary match {
      case PlaceSummary(placed, _, _) => PlaceLettersMove(game, placed).validate flatMap (_.makeMove)
      case SkippedSummary => PassMove(game).makeMove
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

    val it = logInOrder.list.iterator.scanLeft(Try(startGame)) {
      case (tryGame, summary) => tryGame.flatMap(g => replayMove(g, summary))
    }

    it collect { case (Success(x)) => x }
  }

}