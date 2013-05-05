package scrabble

import scala.util.{ Try, Success, Failure }

abstract class Move(game: Game) {

  def makeMove: Try[Game]

}

case class PlaceLettersMove(game: Game, placed: List[(Pos, Tile)]) extends Move(game) {

  //@TODO:Think about how to record games. Tidy up buildWords function. Test it - properly.

  /** Processes the placed letters. Sorts them into positional order. */
  private lazy val placedProcessed = placed.sortBy { case (pos: Pos, let: Tile) => (pos.x, pos.y) }

  /**
   * Returns the updated game if the move is a valid scrabble move, otherwise returns an InvalidMove
   * with an explanation of why the move is invalid
   */
  def makeMove: Try[(Game)] = {

    if (!obeysFirstMovePositionRule) Failure(FirstMovePositionWrong()) else {
      if (!alreadyOccupiedSquares.isEmpty) Failure(SquareOccupiedClientError()) else {

        score.flatMap {
          scr =>
            placeLetters.map {
              case (board, player) =>
                // give the player letters
                val (given, newbag) = game.bag.remove(amountPlaced)
                val newplayer = player.copy(letters = player.letters ++ given, score = player.score + scr.overAllScore)
                val nextPlayer = game.nextPlayerNo
                val players = game.players.updated(game.playersMove, newplayer)
                game.copy(players = players, board = board, playersMove = nextPlayer, bag = newbag, moves = game.moves + 1)
            }
        }
      }
    }

  }

  /** Removes letters from player's letter rack and updates the board. Returns an error if the player does not have the letters  */
  private lazy val placeLetters: Try[(Board, Player)] = {
    val currentPlayer = game.currentPlayer
    val playerLetters = currentPlayer.letters

    def place(placed: List[(Pos, Tile)], remainingLetters: List[Tile], board: Board): Try[(Board, Player)] = {
      placed match {
        case y :: rest =>
          /* Split the remaining player's letters up till it matches one of their placed letters. */
          val (upTo, after) = remainingLetters.span { let => let != y._2 }

          if (upTo.size == remainingLetters.size) Failure(playerDoesNotHaveLettersClientError()) else {
            val newLetters: List[Tile] = upTo ::: after.drop(1)
            place(rest, newLetters, board.placeLetter(y._1, y._2))
          }
        case Nil => Success(board, currentPlayer.replaceLetters(remainingLetters))
      }
    }
    place(placedProcessed, playerLetters, board)
  }

  /**
   * Returns either a list of lists of (Pos, Tile) which are the words (with position info preserved) formed by the
   *  placement of the letters or an error if the player has not placed the words linearly or the letters are not attached
   *   to at least one existing letter on the board
   */
  lazy val formedWords: Try[List[List[(Pos, Tile)]]] = buildWords

  /** Returns an overall score, and a score for each word. Returns a list of words that are not in the dictionary (empty if none) */
  lazy val score: Try[Score] = {
    def toWord(list: List[(Pos, Tile)]): String = list.map { case (pos, letter) => letter.letter }.mkString

    formedWords.flatMap {
      lists =>
        val (score, lsts, badwords) = lists.foldLeft((0, List.empty[(String, Int)], List.empty[String])) {
          case ((acc, lsts, badwords), (xs)) =>
            val word = toWord(xs)
            val bdwords = if (!game.dictionary.isValidWord(word)) word :: badwords else badwords
            val squares = xs.map { case (pos, let) => pos -> board.squareAt(pos).setLetter(let) }

            // Sort to put the word bonus squares last
            val score = squares.sortWith { case ((pos, sq), (pos2, sq2)) => sq < sq2 }.foldLeft(0) {
              case (scr, (pos, sq)) =>
                val square = board.squareAt(pos)

                // If the bonus has already been used, ignore the bonus square
                if (!board.squareAt(pos).isEmpty) scr + square.tile.get.value else {

                  sq match {
                    case (NormalSquare(x)) => scr + sq.tile.get.value
                    case (DoubleLetterSquare(x)) => scr + (sq.tile.get.value * 2)
                    case (TripleLetterSquare(x)) => scr + (sq.tile.get.value * 3)
                    case (DoubleWordSquare(x)) => (scr + sq.tile.get.value) * 2
                    case (TripleWordSquare(x)) => (scr + sq.tile.get.value) * 3
                  }
                }

            }

            (acc + score, lsts :+ (word, score), bdwords)

        }
        val the_score: Score = if (sevenLetterBonus) Score(score + 50, lsts) else Score(score, lsts)

        if (badwords.isEmpty) Success(the_score) else Failure(WordsNotInDictionary(badwords, the_score))

    }

  }
  private lazy val alreadyOccupiedSquares = placed.find { case (pos: Pos, letter: Tile) => !(board.squareAt(pos).isEmpty) }
  private lazy val obeysFirstMovePositionRule = if (game.moves > 0) true else if (game.moves == 0 && placedProcessed(0)._1 == startPosition) true else false
  private lazy val startPosition = Pos.posAt(8, 8).get
  private lazy val sevenLetterBonus: Boolean = amountPlaced == 7
  private val board = game.board
  private lazy val first = placedProcessed(0)

  private lazy val amountPlaced = placedProcessed.size

  private lazy val (startx, endx) = (placedProcessed(0)._1.x, placedProcessed(amountPlaced - 1)._1.x)
  private lazy val (starty, endy) = (placedProcessed(0)._1.y, placedProcessed(amountPlaced - 1)._1.y)
  private lazy val (horizontal, vertical) = (starty == endy, startx == endx)

  // @TODO: Absolutely hurrendous looking. Need to tidy it up.
  private def buildWords: Try[List[List[(Pos, Tile)]]] = {

    def afterEnd(pos: Pos) = (if ((pos.x, pos.y) == (endx, endy)) (if (horizontal) board.LettersRight(pos) else board.LettersAbove(pos)) else List.empty[(Pos, Tile)])

    /** Returns words that are formed from the placement of a letter on a square on the board */
    def allAdjacentTo(pos: Pos, let: Tile): List[(Pos, Tile)] = {
      lazy val above = board.LettersAbove(pos)
      lazy val below = board.LettersBelow(pos)
      lazy val left = board.LettersLeft(pos)
      lazy val right = board.LettersRight(pos)

      if (horizontal) {
        if (!above.isEmpty || !below.isEmpty) {
          above ::: pos -> let :: below
        } else Nil
      } else {
        if (!left.isEmpty || !right.isEmpty) {
          left ::: pos -> let :: right
        } else Nil
      }
    }

    if (!horizontal && !vertical) Failure(NotLinear()) else {

      val startList: List[(Pos, Tile)] = (if (horizontal) board.LettersLeft(placedProcessed(0)._1) else
        board.LettersBelow(placedProcessed(0)._1)) :+ (first._1, first._2)
      val otherWords = allAdjacentTo(first._1, first._2)
      val startWith: List[List[(Pos, Tile)]] = if (otherWords.isEmpty) List(startList) else List(startList) :+ otherWords

      println("startList " + startList)

      val lists: (Int, Int, List[List[(Pos, Tile)]]) = placedProcessed.tail.foldLeft(startx, starty, startWith) {
        case ((lastx, lasty, (x :: xs)), (pos: Pos, let)) =>
          val isLinear = if (horizontal) pos.y == lasty else pos.x == lastx
          if (!isLinear) Failure(MisPlacedLetters(pos.x, pos.y))

          val comesAfter = if (horizontal) pos.x == lastx + 1 else pos.y == lasty + 1

          if (comesAfter) {
            // Add the letter to the first list
            val newlist: List[(Pos, Tile)] = x ::: pos -> let :: afterEnd(pos)
            val updatedList = newlist :: xs
            val otherWords = allAdjacentTo(pos, let)

            println("otherwords" + otherWords)
            (pos.x, pos.y, if (!otherWords.isEmpty) updatedList :+ otherWords else updatedList)

          } else {
            val range = if (horizontal) List.range(lastx + 1, pos.x) else List.range(lasty + 1, pos.y)

            // Add the letters inbetween and the current char to the first list, then look for letters above and below the current char
            val between: List[(Pos, Tile)] = range.map {
              println("Pos is " + pos)
              x =>
                val position = if (horizontal) Pos.posAt(x, pos.y) else Pos.posAt(pos.x, x)
                if (board.squareAt(position.get).isEmpty) Failure(MisPlacedLetters(pos.x, pos.y))
                val sq = board.squareAt(position.get)
                Pos.posAt(pos.x, x).get -> sq.tile.get
            }

            val newlist: List[(Pos, Tile)] = ((x ::: between)) ::: pos -> let :: afterEnd(pos)
            val updatedList = newlist :: xs
            val otherWords: List[(Pos, Tile)] = allAdjacentTo(pos, let)

            (pos.x, pos.y, if (!otherWords.isEmpty) updatedList :+ otherWords else updatedList)
          }

      }

      if (lists._3.size <= 1 && game.moves >= 1) Failure(NotAttachedToWord()) else Success(lists._3)
    }
  }

}

case class PassMove(game: Game) extends Move(game) {
  def makeMove: Try[Game] = ???
}

case class ExchangeMove(game: Game, exchangeLetters: List[Tile]) extends Move(game) {
  def makeMove: Try[Game] = ???
}

object Main {
  def main(args: Array[String]) {
    val game = Game.make(List("jim", "joe"), Dictionary.load("Dict/en.txt"), LetterBag.init).get

    val board = Board.init

    val newBrd = board.squares ++ List(
      Pos.posAt(1, 3).get -> NormalSquare(Some(Letter('S', 1))),
      Pos.posAt(1, 4).get -> NormalSquare(Some(Letter('T', 1))))

    val testBoard = Board(newBrd)

    println(testBoard)

    val placed = List(
      Pos.posAt(1, 1).get -> Letter('L', 1),
      Pos.posAt(1, 2).get -> Letter('G', 1))
    // Pos.posAt(1, 5).get -> Tile('D', 1))

    val blanks = Nil

    val move = PlaceLettersMove((game.copy(board = testBoard)), placed)
    val words = (move.formedWords)

    println(move.score)
  }
}


