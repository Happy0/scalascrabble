package scrabble

import scala.util.{ Try, Success, Failure }

case class Move(game: Game, placed: List[(Pos, Letter)], blanks: List[(Pos, Char)]) {

  //@TODO: Incorporate blanks. Think about how to record games. Tidy up buildWords function. Test it - properly.

  /** Returns the updated game if the move is a valid scrabble move, otherwise returns an InvalidMove with an explanation of why the move is invalid */
  def makeMove: Try[Game] = {

    if (!obeysFirstMovePositionRule) throw (FirstMovePositionWrong()) else {
      if (!alreadyOccupiedSquares.isEmpty) throw (SquareOccupiedClientError()) else {
        val words = Try(formedWords)
        val score = words.flatMap(suc => calculateScores(suc.get))
        lazy val placeLets = placeLetters

        score.flatMap {
          scr =>
            val score = scr
            placeLets.flatMap {
              case (board, player) =>
                // give the player letters
                val (given, newbag) = game.bag.remove(amountPlaced)
                val newplayer = player.replaceLetters(player.letters ++ given).copy(score = player.score + score.overAllScore)
                val nextPlayer = (game.playersMove + 1) % game.players.size // Check maths
                val players = game.players.patch(game.playersMove, Seq(newplayer), 1)
                Success(game.copy(players = players, board = board, playersMove = nextPlayer, bag = newbag, moves = game.moves + 1, score = game.score + score.overAllScore))
            }
        }
      }
    }

  }

  /** Removes letters from player's letter rack and updates the board. Returns an error if the player does not have the letters  */
  def placeLetters: Try[(Board, Player)] = {
    val currentPlayer = game.currentPlayer
    val playerLetters = currentPlayer.letters

    def place(placed: List[(Pos, Letter)], remainingLetters: List[Letter], board: Board): Try[(Board, Player)] = {
      placed match {
        case y :: rest =>
          val (upTo, after) = remainingLetters.span { let => let != y._2 }

          if (upTo.size == remainingLetters.size) throw (playerDoesNotHaveLettersClientError()) else {
            val newLetters: List[Letter] = upTo ::: after.drop(1)
            place(rest, newLetters, board.placeLetter(y._1, after.head))
          }

        case Nil => Success(board, currentPlayer.replaceLetters(remainingLetters))

      }

    }
    place(placed, playerLetters, board)
  }

  /** Returns an overall score, and a score for each word. Returns a list of words that are not in the dictionary (empty if none) */
  def calculateScores(lists: List[List[(Pos, Letter)]]): Try[Score] = {
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

    if (badwords.isEmpty) Success(the_score) else throw WordsNotInDictionary(badwords, the_score)

  }

  private def toWord(list: List[(Pos, Letter)]): String = list.map { case (pos, letter) => letter.letter }.mkString

  lazy val alreadyOccupiedSquares = placed.find { case (pos: Pos, letter: Letter) => !(board.squareAt(pos).isEmpty) }
  lazy val obeysFirstMovePositionRule = if (game.moves > 0) true else if (game.moves == 0 && placedSorted(0)._1 == startPosition) true else false

  lazy val formedWords = buildWords
  lazy val startPosition = Pos.posAt(8, 8).get
  lazy val sevenLetterBonus: Boolean = amountPlaced == 7
  val board = game.board
  lazy val placedMap = placed.toMap
  lazy val placedSorted = placed.sortBy { case (pos: Pos, let: Letter) => (pos.x, pos.y) }
  lazy val first = placedSorted(0)

  lazy val amountPlaced = placedSorted.size

  private lazy val (startx, endx) = (placedSorted(0)._1.x, placedSorted(amountPlaced - 1)._1.x)
  private lazy val (starty, endy) = (placedSorted(0)._1.y, placedSorted(amountPlaced - 1)._1.y)
  private lazy val (horizontal, vertical) = (starty == endy, startx == endx)

  // @TODO: Absolutely hurrendous looking. Need to tidy it up.

  /**
   * Returns either a list of lists of (Pos, Letter) which are the words (with position info preserved) formed by the placement of the letters or an error
   *   if the player has not placed the words linearly or the letters are not attached to at least one existing letter on the board
   */
  def buildWords: Try[List[List[(Pos, Letter)]]] = {
    if (!horizontal && !vertical) throw NotLinear() else {

      val startList: List[(Pos, Letter)] = (if (horizontal) board.LettersLeft(placedSorted(0)._1) else
        board.LettersBelow(placedSorted(0)._1)) :+ (first._1, first._2)

      val otherWords = allAdjacentTo(first._1, first._2)

      val startWith: List[List[(Pos, Letter)]] = if (otherWords.isEmpty) List(startList) else List(startList) :+ otherWords

      println("startList " + startList)

      val lists: (Int, Int, List[List[(Pos, Letter)]]) = placedSorted.tail.foldLeft(startx, starty, startWith) {
        case ((lastx, lasty, (x :: xs)), (pos: Pos, let)) =>
          val isLinear = if (horizontal) pos.y == lasty else pos.x == lastx
          if (!isLinear) return throw (MisPlacedLetters(pos.x, pos.y))

          val comesAfter = if (horizontal) pos.x == lastx + 1 else pos.y == lasty + 1

          if (comesAfter) {
            // Add the letter to the first list
            val newlist: List[(Pos, Letter)] = x ::: pos -> let :: (if ((pos.x, pos.y) == (endx, endy)) (if (horizontal) board.LettersRight(pos) else board.LettersAbove(pos)) else List.empty[(Pos, Letter)])
            val updatedList = newlist :: xs
            val otherWords = allAdjacentTo(pos, let)

            println("otherwords" + otherWords)
            (pos.x, pos.y, if (!otherWords.isEmpty) updatedList :+ otherWords else updatedList)

          } else {
            val range = if (horizontal) List.range(lastx + 1, pos.x) else List.range(lasty + 1, pos.y)

            // Add the letters inbetween and the current char to the first list, then look for letters above and below the current char
            val between: List[(Pos, Letter)] = range.map {
              println("Pos is " + pos)
              x =>
                val position = if (horizontal) Pos.posAt(x, pos.y) else Pos.posAt(pos.x, x)
                if (board.squareAt(position.get).isEmpty) return throw (MisPlacedLetters(pos.x, pos.y))
                val sq = board.squareAt(position.get)
                Pos.posAt(pos.x, x).get -> sq.tile.get
            }

            val newlist: List[(Pos, Letter)] = ((x ::: between)) ::: pos -> let :: (if ((pos.x, pos.y) == (endx, endy)) (if (horizontal) board.LettersRight(pos) else board.LettersAbove(pos)) else List.empty[(Pos, Letter)])
            val updatedList = newlist :: xs
            val otherWords: List[(Pos, Letter)] = allAdjacentTo(pos, let)

            (pos.x, pos.y, if (!otherWords.isEmpty) updatedList :+ otherWords else updatedList)
          }

      }
      if (game.moves >= 1 && lists._3.size <= 1) Right(NotAttachedToWord) else Left(lists._3)

      Success(lists._3)
    }

  }

  /** Returns words that are formed from the placement of a letter on a square on the board */
  def allAdjacentTo(pos: Pos, let: Letter): List[(Pos, Letter)] = {
    lazy val above = board.LettersAbove(pos)
    lazy val below = board.LettersBelow(pos)
    lazy val left = board.LettersLeft(pos)
    lazy val right = board.LettersRight(pos)

    if (horizontal) {
      val list = if (!above.isEmpty || !below.isEmpty) {
        above ::: pos -> let :: below
      } else List()

      list

    } else {
      val list = if (!left.isEmpty || !right.isEmpty) {
        left ::: pos -> let :: right
      } else List()

      list
    }

  }

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
      Pos.posAt(1, 2).get -> Letter('A', 1))
    // Pos.posAt(1, 5).get -> Letter('D', 1))

    val blanks = List()

    val move = Move((game.copy(board = testBoard)), placed, blanks)
    println(move.formedWords)
  }
}


