package scrabble

case class Move(game: Game, placed: List[(Pos, Letter)], blanks: List[(Pos, Char)]) {

  /** Returns the updated game if the move is a valid scrabble move, otherwise returns a String with an explanation of why the move is invalid */
  def updatedGame: Either[InvalidMove, Game] = ???

  // Paranoid checks

  private lazy val playerHasLetters: Boolean = {
    val doesntHave = placed.find { case (Pos(x, y, gr), let) => !game.currentPlayer.letters.contains(let) }
    doesntHave match {
      case None => true
      case Some(x) => false
    }

  }

  lazy val startPosition = Pos.posAt(8, 8).get

  lazy val alreadyOccupiedSquares = placed.find { case (pos: Pos, letter: Letter) => !(board.squareAt(pos).isEmpty) }

  lazy val obeysFirstMovePositionRule = if (game.moves > 0) true else if (game.moves == 0 && placedSorted(0)._1 == startPosition) true else false

  // <Paranoid checks>

  val board = game.board

  lazy val placedMap = placed.toMap
  lazy val placedSorted = placed.sortBy { case (pos: Pos, let: Letter) => (pos.x, pos.y) }

  val amountPlaced = placedSorted.size

  private val (startx, endx) = (placedSorted(0)._1.x, placedSorted(amountPlaced - 1)._1.x)
  private val (starty, endy) = (placedSorted(0)._1.y, placedSorted(amountPlaced - 1)._1.y)
  private val (horizontal, vertical) = (starty == endy, startx == endx)

  // @TODO: Tidy this up. Most hurrendous looking code ever...
  def buildWords: Either[List[List[(Pos, Letter)]], InvalidMove] = {
    if (!horizontal && !vertical) Right(NotLinear()) else {

      val startList: List[(Pos,Letter)] = (if (horizontal) board.LettersLeft(placedSorted(0)._1).map { case (pos, sq) => pos -> sq.tile.get } else
        board.LettersBelow(placedSorted(0)._1).map { case (pos, sq) => pos -> sq.tile.get }) :+ (placedSorted(0)._1, placedSorted(0)._2)

      val otherWords = allAdjacentTo(placedSorted(0)._1, placedSorted(0)._2)

      val startWith: List[List[(Pos,Letter)]] = if (otherWords.isEmpty) List(startList) else List(startList) :+ otherWords

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

      Left(lists._3)
    }

  }

  def allAdjacentTo(pos: Pos, let: Letter): List[(Pos, Letter)] = {
    lazy val above = board.LettersAbove(pos)
    lazy val below = board.LettersBelow(pos)
    lazy val left = board.LettersBelow(pos)
    lazy val right = board.LettersRight(pos)

    if (horizontal) {
      val list = if (!above.isEmpty || !below.isEmpty) (above.map { case (ps, sq) => ps -> sq.tile.get } :+ pos -> let) ::: below.map { case (ps, sq) => ps -> sq.tile.get } else List()

      if ((pos.x, pos.y) == (endx, endy)) list ::: right.map { case (ps, sq) => ps -> sq.tile.get } else list

    } else {
      val list = if (!left.isEmpty || !right.isEmpty) (left.map { case (ps, sq) => ps -> sq.tile.get } :+ pos -> let) ::: right.map { case (ps, sq) => ps -> sq.tile.get } else List()

      if ((pos.x, pos.y) == (endx, endy)) list ::: above.map { case (ps, sq) => ps -> sq.tile.get } else list
    }

  }

}

object Main {
  def main(args: Array[String]) {
    val game = Game.init(List("jim", "joe"), Dictionary.load("C:\\Users\\Gordon\\workspace\\scalascrabble\\src\\Dict\\en.txt"), LetterBag.init)

    val board = Board.init

    val newBrd = board.squares + (Pos.posAt(2, 2).get -> NormalSquare(Some(Letter('a', 1))))
    val testBoard = Board(newBrd)

    println(testBoard)

    val placed = List(
      Pos.posAt(1, 2).get -> Letter('A', 1),
      Pos.posAt(1, 3).get -> Letter('A', 1),
      Pos.posAt(1, 4).get -> Letter('A', 1))

    val blanks = List()

    val move = Move(Game(game.players, testBoard, game.playersMove, game.bag, 0), placed, blanks)
    println(move.placedSorted)

    println(move.buildWords)
  }
}


