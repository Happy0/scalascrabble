package scrabble

case class Move(game: Game, placed: List[(Pos, Letter)], blanks: List[(Pos, Char)]) {

  /** Returns the updated game if the move is a valid scrabble move, otherwise returns an InvalidMove with an explanation of why the move is invalid */
  def updatedGame: Either[InvalidMove, Game] = {
    val checks = moveError

    checks match {
      case Some(error) => Left(error)

      // Move is valid, update the game state 
      case None =>
        ???
    }
  }

  /** Removes letters from player's letter rack and updates the board. Returns an error if the player does not have the letters  */
  def placeLetters: Either[(Board, Player), InvalidMove] = {
    ???
  }

  /** Returns an overall score, and a score for each word */
  def calculateScores(lists: List[List[(Pos, Letter)]]): (Int, List[(String, Int)]) = {
    val (score, lsts) = lists.foldLeft((0, List.empty[(String, Int)])) {
      case ((acc, lsts), (xs)) =>
        
        val word = toWord(xs)
        
        val squares = xs.map{case (pos, let) => board.squareAt(pos).setLetter(let)}

        // Sort to put the word bonus squares last
        val score = squares.sortWith(_ < _).foldLeft(0) {
          case (scr, sq) =>
            sq match {
              case (NormalSquare(x)) => scr + sq.tile.get.value
              case (DoubleLetterSquare(x)) => scr + (sq.tile.get.value * 2)
              case (TripleLetterSquare(x)) => scr + (sq.tile.get.value * 3)
              case (DoubleWordSquare(x)) => (scr + sq.tile.get.value) * 2
              case (TripleWordSquare(x)) => (scr + sq.tile.get.value) * 3
            }

        }
        
        (acc + score, lsts :+ (word, score))

    }
    
    if (sevenLetterBonus) (score + 50, lsts) else (score, lsts)

  }

  /** Returns an InvalidMove object describing the error in the move, or returns None if there is no error in the placement of letters. */
  val moveError: Option[InvalidMove] = {
    if (!obeysFirstMovePositionRule) Some(FirstMovePositionWrong()) else {
      if (!alreadyOccupiedSquares.isEmpty) Some(SquareOccupiedClientError()) else {

        formedWords match {
          case Right(error) => Some(error)
          case Left(list) =>
            val badwords = badWords(list)
            if (!badwords.isEmpty) {
              Some(WordsNotInDictionary(badwords))
            } else {
              None
            }

        }
      }

    }
  }

  def toWord(list: List[(Pos, Letter)]): String = list.map { case (pos, letter) => letter.letter }.mkString

  lazy val alreadyOccupiedSquares = placed.find { case (pos: Pos, letter: Letter) => !(board.squareAt(pos).isEmpty) }
  lazy val obeysFirstMovePositionRule = if (game.moves > 0) true else if (game.moves == 0 && placedSorted(0)._1 == startPosition) true else false

  def badWords(crawled: List[List[(Pos, Letter)]]): List[String] = {
    val words: List[String] = crawled.foldLeft(List.empty[String]) {
      case (wordList, list) =>
        toWord(list) :: wordList
    }
    println(words)

    game.dictionary.invalidWords(words)
  }

  lazy val formedWords = buildWords
  lazy val startPosition = Pos.posAt(8, 8).get
  lazy val sevenLetterBonus: Boolean = amountPlaced == 7
  val board = game.board
  lazy val placedMap = placed.toMap
  lazy val placedSorted = placed.sortBy { case (pos: Pos, let: Letter) => (pos.x, pos.y) }
  val amountPlaced = placedSorted.size
  private lazy val (startx, endx) = (placedSorted(0)._1.x, placedSorted(amountPlaced - 1)._1.x)
  private lazy val (starty, endy) = (placedSorted(0)._1.y, placedSorted(amountPlaced - 1)._1.y)
  private lazy val (horizontal, vertical) = (starty == endy, startx == endx)

  /**
   * Returns either a list of lists of (Pos, Letter) which are the words (with position info preserved) formed by the placement of the letters or an error
   *   if the player has not placed the words linearly or the letters are not attached to at least one existing letter on the board
   */
  def buildWords: Either[List[List[(Pos, Letter)]], InvalidMove] = {
    if (!horizontal && !vertical) Right(NotLinear()) else {

      val startList: List[(Pos, Letter)] = (if (horizontal) board.LettersLeft(placedSorted(0)._1) else
        board.LettersBelow(placedSorted(0)._1)) :+ (placedSorted(0)._1, placedSorted(0)._2)

      val otherWords = allAdjacentTo(placedSorted(0)._1, placedSorted(0)._2)

      val startWith: List[List[(Pos, Letter)]] = if (otherWords.isEmpty) List(startList) else List(startList) :+ otherWords

      println("startList " + startList)

      val lists: (Int, Int, List[List[(Pos, Letter)]]) = placedSorted.tail.foldLeft(startx, starty, startWith) {
        case ((lastx, lasty, (x :: xs)), (pos: Pos, let)) =>
          val isLinear = if (horizontal) pos.y == lasty else pos.x == lastx
          if (!isLinear) return Right(MisPlacedLetters(pos.x, pos.y))

          val comesAfter = if (horizontal) pos.x == lastx + 1 else pos.y == lasty + 1

          if (comesAfter) {
            // Add the letter to the first list
            val newlist = x :+ pos -> let
            val updatedList = newlist :: xs
            val otherWords = allAdjacentTo(pos, let)

            println("otherwords" + otherWords)
            (pos.x, pos.y, if (!otherWords.isEmpty) updatedList :+ otherWords else updatedList)

          } else {
            val range = if (horizontal) List.range(lastx + 1, pos.x) else List.range(lasty + 1, pos.y)

            // Add the letters inbetween and the current char to the first list, then look for letters above and below the current char
            val between = range.map {
              x =>
                if (board.squareAt(pos).isEmpty) return Right(MisPlacedLetters(pos.x, pos.y))
                val position = if (horizontal) Pos.posAt(x, pos.y) else Pos.posAt(pos.x, x)
                val sq = board.squareAt(position.get)
                Pos.posAt(pos.x, x).get -> sq
            } :+ pos -> let.letter

            val newlist = x :+ pos -> let
            val updatedList = newlist :: xs
            val otherWords: List[(Pos, Letter)] = allAdjacentTo(pos, let)

            (pos.x, pos.y, if (!otherWords.isEmpty) updatedList :+ otherWords else updatedList)
          }

      }
      if (game.moves >= 1 && lists._3.size <= 1) Right(NotAttachedToWord) else Left(lists._3)

      Left(lists._3)
    }

  }

  /** Returns words that are formed from the placement of a letter on a square on the board */
  def allAdjacentTo(pos: Pos, let: Letter): List[(Pos, Letter)] = {
    lazy val above = board.LettersAbove(pos)
    lazy val below = board.LettersBelow(pos)
    lazy val left = board.LettersBelow(pos)
    lazy val right = board.LettersRight(pos)

    if (horizontal) {
      val list = if (!above.isEmpty || !below.isEmpty) {
        (above :+ pos -> let) ::: below
      } else List()

      if ((pos.x, pos.y) == (endx, endy)) list ::: right else list

    } else {
      val list = if (!left.isEmpty || !right.isEmpty) {
        (left :+ pos -> let) ::: right
      } else List()

      if ((pos.x, pos.y) == (endx, endy)) list ::: above else list
    }

  }

}

object Main {
  def main(args: Array[String]) {
    val game = Game.init(List("jim", "joe"), Dictionary.load("C:\\Users\\Gordon\\workspace\\scalascrabble\\src\\Dict\\en.txt"), LetterBag.init)

    val board = Board.init

    val newBrd = board.squares + (Pos.posAt(2, 4).get -> NormalSquare(Some(Letter('a', 1))))
    val testBoard = Board(newBrd)

    println(testBoard)

    val placed = List(
      Pos.posAt(1, 2).get -> Letter('A', 1),
      Pos.posAt(1, 3).get -> Letter('A', 1),
      Pos.posAt(1, 4).get -> Letter('A', 1))

    val blanks = List()

    val move = Move((game.copy(board = testBoard)), placed, blanks)
    println(move.placedSorted)
  }
}


