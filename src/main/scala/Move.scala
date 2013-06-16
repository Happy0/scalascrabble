package scrabble

import scala.util.{ Try, Success, Failure }
import scalaz.NonEmptyList
import scalaz.Lists

abstract class Move(game: Game) {

  def makeMove: Try[Game]

}

case class PlaceLettersMove(game: Game, placed: NonEmptyList[(Pos, Tile)]) extends Lists {

  def validate: Try[ValidPlaceLettersMove] = {

    // Makes sure the tiles are sorted into ascending order of position
    val sortedList = placed.list.sortBy { case (pos: Pos, _) => (pos.x, pos.y) }

    def unwrap(pos: Pos, tile: Tile): Option[(Pos, Square, Tile)] = game.board.squareAt(pos) map { sq => (pos, sq, tile) }

    def processed: Option[NonEmptyList[(Pos, Square, Tile)]] = {
      val unwrapped = sortedList map { case (pos, tile) => unwrap(pos, tile) } sequence

      unwrapped flatMap (list => list.toNel)
    }

    if (game.status != InProgress) Failure(GameHasEnded()) else
      processed.toTry(Failure(UnlikelyInternalError())) { list => Try(ValidPlaceLettersMove(game, list)) }
  }

}

sealed case class ValidPlaceLettersMove(game: Game, placed: NonEmptyList[(Pos, Square, Tile)]) extends Move(game) {

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
      if (!alreadyOccupiedSquares.isEmpty) Failure(SquareOccupiedClientError()) else {

        score.flatMap {
          scr =>
            placeLetters.map {
              case (board, player) =>
                // give the player letters
                val (given, newbag) = game.bag.remove(amountPlaced)
                val newplayer = player.copy(letters = given ::: player.letters, score = player.score + scr.overAllScore)
                val nextPlayer = game.nextPlayerNo
                val players = game.players.updated(game.playersMove, newplayer)
                val updatedGame = game.copy(players = players, board = board, playersMove = nextPlayer, bag = newbag, moves = game.moves + 1,
                  consecutivePasses = 0)

                if (updatedGame.gameEnded) updatedGame.copy(status = Ended) else updatedGame
            }
        }
      }
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

                board.placeLetter(y._1, y._3).toTry(Failure(UnlikelyInternalError())) {
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
  lazy val formedWords: Try[List[List[(Pos, Square, Tile)]]] = buildWords

  /** Returns an overall score, and a score for each word. Returns a list of words that are not in the dictionary (empty if none) */
  lazy val score: Try[Score] = {
    def toWord(list: List[(Pos, Square, Tile)]): String = list.map { case (pos, sq, tile) => tile.letter }.mkString

    formedWords.flatMap {
      lists =>
        val (score, lsts, badwords) = lists.foldLeft((0, List.empty[(String, Int)], List.empty[String])) {
          case ((acc, lsts, badwords), xs) =>
            val word = toWord(xs)
            val bdwords = if (!game.dictionary.isValidWord(word)) word :: badwords else badwords
            // Sort to put the word bonus squares last
            val (score, wordBonuses) = xs.foldLeft(0, List.empty[Int => Int]) {
              case ((scr, wordBonuses), (pos, sq, tile)) =>

                // If the bonus has already been used, ignore the bonus square
                sq.tile.fold {
                  sq match {
                    case NormalSquare(_) => (scr + tile.value, wordBonuses)
                    case DoubleLetterSquare(_) => (scr + (tile.value * 2), wordBonuses)
                    case TripleLetterSquare(_) => (scr + (tile.value * 3), wordBonuses)
                    case DoubleWordSquare(_) => (scr + tile.value, ((i: Int) => i * 2) :: wordBonuses)
                    case TripleWordSquare(_) => (scr + tile.value, ((i: Int) => i * 3) :: wordBonuses)
                  }
                }(currentTile =>
                  (scr + currentTile.value, wordBonuses))
            }

            val finalScore = wordBonuses.foldLeft(score) { case (score, func) => func(score) }

            (acc + finalScore, lsts :+ (word, finalScore), bdwords)

        }
        val the_score: Score = if (sevenLetterBonus) Score(score + 50, lsts) else Score(score, lsts)

        if (badwords.isEmpty) Success(the_score) else Failure(WordsNotInDictionary(badwords, the_score))

    }

  }

  private lazy val obeysFirstMovePositionRule = (game.moves > 0) || {
    placedProcessed.find { case (pos, _, let) => pos == startPosition } isDefined
  }

  private lazy val alreadyOccupiedSquares = placedProcessed.find {
    case (pos: Pos, sq, _) => !(sq.isEmpty)
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
      val horizontal = !board.LettersLeft(first._1).isEmpty || !board.LettersRight(first._1).isEmpty
      val vertical = !board.LettersAbove(first._1).isEmpty || !board.LettersBelow(first._1).isEmpty

      (horizontal, vertical)
    } else (starty == endy, startx == endx)
  }

  // @TODO: Absolutely hurrendous looking. Need to tidy it up.
  private def buildWords: Try[List[List[(Pos, Square, Tile)]]] = {

    def isLastPlaced(pos: Pos): Boolean = pos.x == endx && pos.y == endy

    def afterEnd(pos: Pos) =
      if (isLastPlaced(pos)) {
        horizontalElseVertical(board.LettersRight(pos))(board.LettersAbove(pos))
      } else Nil

    /** Returns words that are formed from the placement of a letter on a square on the board */
    def allAdjacentTo(pos: Pos, sq: Square, let: Tile): List[(Pos, Square, Tile)] = {
      lazy val above = board.LettersAbove(pos)
      lazy val below = board.LettersBelow(pos)
      lazy val left = board.LettersLeft(pos)
      lazy val right = board.LettersRight(pos)

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

      //@TODO: Tidy this up. My god. 
      val startList: List[(Pos, Square, Tile)] =
        ((horizontalElseVertical(board.LettersLeft(first._1))(board.LettersBelow(first._1))) :+ (first._1, first._2, first._3)) ::: afterEnd(first._1)
      val otherWords = allAdjacentTo(first._1, first._2, first._3)
      val startWith: List[List[(Pos, Square, Tile)]] = if (otherWords.isEmpty) List(startList) else List(startList) :+ otherWords

      def findWords(placed: List[(Pos, Square, Tile)], lastPos: Pos,
        startWith: List[List[(Pos, Square, Tile)]]): Try[List[List[(Pos, Square, Tile)]]] = {

        (placed, startWith) match {
          case (_, Nil) => Failure(UnlikelyInternalError())
          case (Nil, xs) => Success(xs)

          case (((pos, sq, let) :: rest), (x :: xs)) =>

            val isLinear = horizontalElseVertical(pos.y == lastPos.y)(pos.x == lastPos.x)
            if (!isLinear) Failure(MisPlacedLetters(pos.x, pos.y))

            val comesAfter = horizontalElseVertical(pos.x == lastPos.x + 1)(pos.y == lastPos.y + 1)

            if (comesAfter) {
              // Add the letter to the first list
              val newlist: List[(Pos, Square, Tile)] = x ::: (pos, sq, let) :: afterEnd(pos)
              val updatedList = newlist :: xs
              val otherWords = allAdjacentTo(pos, sq, let)

              findWords(rest, pos, if (!otherWords.isEmpty) updatedList :+ otherWords else updatedList)

            } else {

              // val range = horizontalElseVertical(List.range(lastx + 1, pos.x))(List.range(lasty + 1, pos.y))
              val predicesor = horizontalElseVertical(pos.left)(pos.down)
              // Add the letters inbetween and the current char to the first list, then look for letters above and below the current char

              val between = predicesor flatMap {
                predicsorPos =>
                  val between: List[(Pos, Square, Tile)] =
                    horizontalElseVertical(board.LettersRight(lastPos))(board.LettersAbove(lastPos))
                  between.find { case (ps, sq, tile) => ps == predicsorPos } map (_ => between)
              }

              between.toTry {
                Failure(MisPlacedLetters(pos.x, pos.y))
              } {
                case between =>
                  val newlist: List[(Pos, Square, Tile)] = ((x ::: between)) ::: (pos, sq, let) :: afterEnd(pos)
                  val updatedList = newlist :: xs
                  val otherWords: List[(Pos, Square, Tile)] = allAdjacentTo(pos, sq, let)

                  findWords(rest, pos, if (!otherWords.isEmpty) updatedList :+ otherWords else updatedList)
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
        lists =>
          lists match {
            case Nil =>
              Failure(UnlikelyInternalError())
            case (x :: xs) =>
              val attached = x.size > placedProcessed.size || lists.size > 1 || game.moves == 0
              if (attached) Success(lists) else Failure(NotAttachedToWord())
          }
      }

    }
  }

}

case class PassMove(game: Game) extends Move(game) {
  def makeMove: Try[Game] = {
    Success(game.copy(consecutivePasses = game.consecutivePasses + 1, playersMove = game.nextPlayerNo,
      moves = game.moves + 1))
  }

}

case class ExchangeMove(game: Game, exchanged: List[Tile]) extends Move(game) {
  def makeMove: Try[Game] = {
    val newGame = game.currentPlayer map {
      player: Player =>
        game.bag.exchange(exchanged) flatMap {
          case (given, newBag) =>
            player.exchangeLetters(exchanged, given) map {
              ply =>
                game.copy(players = game.players.updated(game.playersMove, ply),
                  playersMove = game.nextPlayerNo, moves = game.moves + 1, bag = newBag, consecutivePasses = 0)
            }

        }
    }
    newGame.fold[Try[Game]](Failure(UnlikelyInternalError()))(a => a)
  }

}


