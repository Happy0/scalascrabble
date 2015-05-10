package scrabble

import scala.util.{ Try, Success, Failure }
import scalaz.NonEmptyList
import scalaz.Lists

abstract class Move(game: Game) {

  def makeMove: Try[Game]

}

case class PlaceLettersMove(game: Game, placed: NonEmptyList[(Pos, Tile)]) extends Lists {

  def validate: Try[ValidInputPlaceLettersMove] = {

    // Makes sure the tiles are sorted into descending order of position
    val sortedList = placed.list.sortBy { case (pos: Pos, _) => (-pos.x, -pos.y) }

    def unwrap(pos: Pos, tile: Tile): Option[(Pos, Square, Tile)] = game.board.squareAt(pos) map { sq => (pos, sq, tile) }

    def processed: Option[NonEmptyList[(Pos, Square, Tile)]] = {
      val unwrapped = sortedList map { case (pos, tile) => unwrap(pos, tile) } sequence

      unwrapped flatMap (list => list.toNel)
    }

    def alreadyOccupiedSquares(list: List[PosSquare]) = {
      list.find {
        case (pos: Pos, sq, _) => !(sq.isEmpty)
      } nonEmpty
    }

    if (game.status != InProgress) Failure(GameHasEnded()) else
      processed.toTry(UnlikelyInternalError()) flatMap { list =>
        if (alreadyOccupiedSquares(list.list)) Failure(SquareOccupiedClientError()) else {
          Try(ValidInputPlaceLettersMove(game, list))
        }
      }
  }

}

sealed case class ValidInputPlaceLettersMove(game: Game, placed: NonEmptyList[(Pos, Square, Tile)]) extends Move(game) {

  def horizontalElseVertical[A](horizontal: => A)(vertical: => A): A = {
    if (this.horizontal) horizontal else vertical
  }

  private lazy val placedProcessed = placed.list

  /**
   * Returns the updated game if the move is a valid scrabble move, otherwise returns an InvalidMove
   * with an explanation of why the move is invalid
   */
  def makeMove: Try[Game] = {

    if (!obeysFirstMovePositionRule) Failure(FirstMovePositionWrong()) else {

      for {
        scr <- score
        formed <- formedWords
        (board, player) <- placeLetters

        (given, newbag) = game.bag.remove(amountPlaced)
        newplayer = player.copy(letters = given ::: player.letters, score = player.score + scr.overAllScore)
        nextPlayer = game.nextPlayerNo
        players = game.players.updated(game.playersMove, newplayer)

        summary = PlaceSummary(placed.map(f => f._1 -> f._3), formed, scr)

        hist = game.log.fold(History(game, NonEmptyList(summary)))(log => log.addToLog(summary))

        updatedGame = game.copy(players = players, board = board, playersMove = nextPlayer, bag = newbag, moves = game.moves + 1,
          consecutivePasses = 0, log = Some(hist))

        newGame = if (updatedGame.gameEnded) updatedGame.copy(status = Ended) else updatedGame

      } yield (newGame)
    }

  }

  /** Removes letters from player's letter rack and updates the board. Returns an error if the player does not have the letters  */
  private lazy val placeLetters: Try[(Board, Player)] = {
    game.currentPlayer.fold[Try[(Board, Player)]](Failure(UnlikelyInternalError())) {
      currentPlayer =>
        val playerLetters = currentPlayer.letters

        def place(placed: List[(Pos, Square, Tile)], remainingLetters: List[Tile], board: Board): Try[(Board, Player)] = {
          placed match {
            case y :: rest =>
              /* Split the remaining player's letters up till it matches one of their placed letters. */
              val (upTo, after) = remainingLetters.span { let => let != y._3 }

              if (after == Nil) Failure(playerDoesNotHaveLettersClientError()) else {
                val newLetters: List[Tile] = upTo ::: after.drop(1)

                board.placeLetter(y._1, y._3).toTry(UnlikelyInternalError()) flatMap {
                  board => place(rest, newLetters, board)
                }

              }
            case Nil => Success(board, currentPlayer.replaceLetters(remainingLetters))
          }
        }
        place(placedProcessed, playerLetters, board)
    }

  }

  /**
   * Returns either a list of lists of (Pos, Tile) which are the words (with position info preserved) formed by the
   *  placement of the letters or an error if the player has not placed the words linearly or the letters are not attached
   *   to at least one existing letter on the board
   */
  lazy val formedWords: Try[FormedWords] = buildWords

  /**
   * Returns an overall score, and a score for each word. Returns a list of words that are not
   *   in the dictionary (empty if none)
   */
  lazy val score: Try[Score] = formedWords.flatMap(_.calculateScore(game.dictionary, sevenLetterBonus))

  private lazy val obeysFirstMovePositionRule = (game.moves > 0) || {
    placedProcessed.find { case (pos, _, let) => pos == startPosition } isDefined
  }

  private lazy val startPosition = Pos.startPosition
  private lazy val sevenLetterBonus: Boolean = amountPlaced == 7
  private val board = game.board

  private lazy val first = placed.head
  private lazy val last = placed.last

  private lazy val amountPlaced = placedProcessed.size

  private lazy val startPos = first._1
  private lazy val endPos = last._1

  private lazy val (startx, endx) = (first._1.x, last._1.x)
  private lazy val (starty, endy) = (first._1.y, last._1.y)

  private lazy val (horizontal, vertical): (Boolean, Boolean) = {
    if (amountPlaced == 1) {
      val horizontal = board.lettersLeft(first._1).nonEmpty || board.lettersRight(first._1).nonEmpty
      val vertical = board.lettersAbove(first._1).nonEmpty || board.lettersBelow(first._1).nonEmpty

      (horizontal, vertical)
    } else (starty == endy, startx == endx)
  }

  private def buildWords: Try[FormedWords] = {

    def isLastPlaced(pos: Pos): Boolean = pos.x == endx && pos.y == endy

    lazy val afterEnd = horizontalElseVertical(board.lettersLeft(last._1))(board.lettersBelow(last._1))

    /** Returns words that are formed from the placement of a letter on a square on the board */
    def allAdjacentTo(pos: Pos, sq: Square, let: Tile): List[(Pos, Square, Tile)] = {
      lazy val above = board.lettersAbove(pos)
      lazy val below = board.lettersBelow(pos)
      lazy val left = board.lettersLeft(pos)
      lazy val right = board.lettersRight(pos)

      horizontalElseVertical {
        if (above.nonEmpty || below.nonEmpty) {
          below ::: (pos, sq, let) :: above
        } else Nil
      } {
        if (left.nonEmpty || right.nonEmpty) {
          left ::: (pos, sq, let) :: right
        } else Nil
      }

    }

    if (!horizontal && !vertical) Failure(NotLinear()) else {

      val mainWordStart: List[PosSquare] =
        first :: ((horizontalElseVertical(board.lettersRight(first._1))(board.lettersAbove(first._1))))
      val adjacentWord = allAdjacentTo(first._1, first._2, first._3)
      val adjStart: List[List[(Pos, Square, Tile)]] = if (adjacentWord.isEmpty) Nil else adjacentWord :: Nil

      val startWith = FormedWords(mainWordStart, adjStart)

      def findWords(placed: List[(Pos, Square, Tile)], lastPos: Pos,
        builder: FormedWords): Try[FormedWords] = {

        (placed, builder) match {
          case (Nil, builder) => Success(builder)

          case (((pos, sq, let) :: rest), builder) =>

            val isLinear = horizontalElseVertical(pos.y == lastPos.y)(pos.x == lastPos.x)
            if (!isLinear) Failure(MisPlacedLetters(pos.x, pos.y))

            val comesAfter = horizontalElseVertical(pos.x == lastPos.x - 1)(pos.y == lastPos.y - 1)

            if (comesAfter) {
              // Add the letter to the first list
              val newbuilder: FormedWords = builder.prependToMainWord((pos, sq, let))
              val adjacent = allAdjacentTo(pos, sq, let)
              val nextbuilder = if (adjacent.nonEmpty) newbuilder.addAdjacentWord(adjacent) else newbuilder

              findWords(rest, pos, nextbuilder)

            } else {
              val predicesor = horizontalElseVertical(lastPos.left)(lastPos.down)

              /* Add the letters inbetween and the current char to the first list,
              * then look for letters above and below the current char */

              val between = predicesor flatMap {
                predicsorPos =>
                  val between =
                    horizontalElseVertical(board.lettersRight(pos))(board.lettersAbove(pos))
                  between.find { case (ps, sq, tile) => ps == predicsorPos } map (_ => between)
              }

              between.toTry {
                MisPlacedLetters(pos.x, pos.y)
              } flatMap {
                case between =>
                  val newBuilder: FormedWords = builder.prependToMainWord((pos, sq, let) :: between)
                  val adjacent: List[(Pos, Square, Tile)] = allAdjacentTo(pos, sq, let)

                  findWords(rest, pos, if (adjacent.nonEmpty) newBuilder.addAdjacentWord(adjacent) else newBuilder)
              }
            }
        }
      }

      // If the placed letters extend a linear word, or are placed at right angles to another word (forming more words)

      val words = placedProcessed match {
        case Nil => Failure(UnlikelyInternalError())
        case x :: xs =>
          findWords(xs, startPos, startWith)
      }

      words flatMap {
        formedWords =>
          val finished = formedWords.prependToMainWord(afterEnd)

          val attached = finished.adjacentWords.nonEmpty || finished.mainWord.size > placedProcessed.size || game.moves == 0
          if (attached) Success(finished) else Failure(NotAttachedToWord())
      }

    }
  }

}

case class PassMove(game: Game) extends Move(game) {
  def makeMove: Try[Game] = {
    lazy val init = History(game, NonEmptyList(SkippedSummary))
    val hist = game.log.fold(init)(log => log.addToLog(SkippedSummary))

    Success(game.copy(consecutivePasses = game.consecutivePasses + 1, playersMove = game.nextPlayerNo,
      moves = game.moves + 1, log = Some(hist)))
  }

}

case class ExchangeMove(game: Game, exchanged: List[Tile]) extends Move(game) {

  def history(newBag: String): History = {
    val summary = ExchangedSummary(exchanged, newBag)
    lazy val init = History(game, NonEmptyList(summary))
    game.log.fold(init)(log => log.addToLog(summary))
  }

  def makeMove: Try[Game] = {
    val newGame = game.currentPlayer map {
      player: Player =>
        game.bag.exchange(exchanged) flatMap {
          case (given, newBag) =>
            player.exchangeLetters(exchanged, given) map {
              ply =>
                game.copy(players = game.players.updated(game.playersMove, ply),
                  playersMove = game.nextPlayerNo, moves = game.moves + 1, bag = newBag, consecutivePasses = 0,
                  log = Some(history(newBag.lettersAsString)))
            }

        }
    }
    newGame.fold[Try[Game]](Failure(UnlikelyInternalError()))(a => a)
  }

}


