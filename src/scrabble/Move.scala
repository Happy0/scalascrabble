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

  val moveError: Option[InvalidMove] = {
    if (!playerHasLetters || !alreadyOccupiedSquares.isEmpty) Some(ClientError()) else {
      if (!obeysFirstMovePositionRule) Some(FirstMovePositionWrong()) else {

        formedWords match {
          case Right(x) => Some(x)
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

  // Paranoid checks

  private lazy val playerHasLetters: Boolean = {

    // @TODO: Argh. What about two of the same letter?
    val doesntHave = placed.find { case (Pos(x, y, gr), let) => !game.currentPlayer.letters.contains(let) }
    doesntHave match {
      case None => true
      case Some(x) => false
    }

  }

  lazy val alreadyOccupiedSquares = placed.find { case (pos: Pos, letter: Letter) => !(board.squareAt(pos).isEmpty) }
  lazy val obeysFirstMovePositionRule = if (game.moves > 0) true else if (game.moves == 0 && placedSorted(0)._1 == startPosition) true else false

  def badWords(crawled: List[List[(Pos, Letter)]]): List[String] = {
    val words: List[String] = crawled.foldLeft(List.empty[String]) {
      case (wordList, list) =>
        list.map { case (pos, letter) => letter.letter }.mkString :: wordList
    }

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
            val emptiesBetween = range.find { nxt =>
              val curPos = if (horizontal) Pos.posAt(nxt, pos.y).get else Pos.posAt(pos.x, nxt).get
              board.squareAt(curPos).isEmpty
            }

            if (!emptiesBetween.isDefined) {
              // Add the letters inbetween and the current char to the first list, then look for letters above and below the current char
              val between = range.map {

                x =>
                  val position = if (horizontal) Pos.posAt(x, pos.y) else Pos.posAt(pos.x, x)
                  val sq = board.squareAt(position.get)

                  Pos.posAt(pos.x, x).get -> sq
              } :+ pos -> let.letter

              val newlist = x :+ pos -> let
              val updatedList = newlist :: xs

              val otherWords: List[(Pos, Letter)] = allAdjacentTo(pos, let)

              (pos.x, pos.y, if (!otherWords.isEmpty) updatedList :+ otherWords else updatedList)

            } else {
              return Right(MisPlacedLetters(pos.x, pos.y))
            }

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
    val game = Game.init(List("jim", "joe"), Dictionary.load("C:\\workspace\\Scala\\scalascrabble\\src\\Dict\\en.txt"), LetterBag.init)

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

    println(move.buildWords)
  }
}


