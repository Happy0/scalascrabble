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

  lazy val alreadyOccupiedSquares = placed.find { case (pos: Pos, letter: Letter) => !(board.squareAt(pos).isEmpty) }

  // <Paranoid checks>

  val board = game.board

  lazy val placedMap = placed.toMap
  val placedSorted = placed.sortBy { case (pos: Pos, let: Letter) => (pos.x, pos.y) }

  val amountPlaced = placedSorted.size

  val (startx, endx) = (placedSorted(0)._1.x, placedSorted(amountPlaced - 1)._1.x)
  val (starty, endy) = (placedSorted(0)._1.y, placedSorted(amountPlaced - 1)._1.y)
  val (horizontal, vertical) = (starty == endy, startx == endx)

  // Returns true if the letter are placed in a legal distribution (linear or horizontal) within the board range, and it is attached to at least one existing word

  //@TODO: Collect words as it goes
  def validlyPlaced: Either[Boolean, InvalidMove] = {

    if (!horizontal && !vertical) false else true

    def isLinear: (Boolean, Boolean, Int, Int) = {

      placedSorted.tail.foldLeft(true, false, startx, starty) {
        case ((bl, neigh, lastx: Int, lasty: Int), (pos, let)) =>
          val isLinear = if (horizontal) pos.y == lasty else pos.x == lastx
          if (!isLinear) return (false, false, pos.x, pos.y)
          val comesAfter = if (horizontal) pos.x == lastx + 1 else pos.y == lasty + 1

          // Search for neighbouring squares
          lazy val lookAt = pos.up :: pos.down :: pos.left :: pos.right :: List()

          val hasNeighbours = if (neigh == true) true else !lookAt.find { ps => if (!ps.isDefined) false else !board.squareAt(ps.get).isEmpty }.isEmpty

          if (comesAfter) (true, hasNeighbours, pos.x, pos.y) else {
            // Loop from the current position to the previous position, making sure there are squares inbetween that the word is built from
            val range = if (horizontal) List.range(lastx + 1, pos.x) else List.range(lasty + 1, pos.y)

            val emptiesBetween = range.find { nxt =>
              val curPos = if (horizontal) Pos.posAt(nxt, pos.y).get else Pos.posAt(pos.x, nxt).get
              board.squareAt(curPos).isEmpty
            }
            println(emptiesBetween)

            if (!emptiesBetween.isDefined) (true, true, pos.x, pos.y) else return (false, false, pos.x, pos.y)

          }

      }

    }

    val (bl, neigh, x, y) = isLinear

    if (bl && neigh) Left(true) else if (!neigh && bl) return Right(NotAttachedToWord()) else Right(MisPlacedLetters(x, y))
  }

  def buildWords: Either[List[List[(Pos, Char)]], InvalidMove] = {
    if (!horizontal && !vertical) Right(NotLinear()) else {

      val startList = if (horizontal) board.LettersLeft(placedSorted(0)._1).map { case (pos, sq) => pos -> sq.tile.get.letter } else
        board.LettersAbove(placedSorted(0)._1).map { case (pos, sq) => pos -> sq.tile.get.letter } :+ (placedSorted(0)._1, placedSorted(0)._2.letter)

      val lists: (Int, Int, List[List[(Pos, Char)]]) = placedSorted.tail.foldLeft(startx, starty, List(startList)) {
        case ((lastx, lasty, (x :: xs)), (pos: Pos, let)) =>
          val isLinear = if (horizontal) pos.y == lasty else pos.x == lastx
          if (!isLinear) return Right(MisPlacedLetters(pos.x, pos.y))

          val comesAfter = if (horizontal) pos.x == lastx + 1 else pos.y == lasty + 1

          if (comesAfter) {
            // Add the letter to the first list
            val newlist = x :+ pos -> let.letter
            val updatedList = newlist :: xs

            val otherWords: List[(Pos, Char)] =
              if (horizontal) {
                println("above: " + board.LettersAbove(pos))
                println("below : " + board.LettersBelow(pos))
                
                (board.LettersAbove(pos).map { case (pos, sq) => pos -> sq.tile.get.letter } :+ pos -> let.letter) ::: board.LettersBelow(pos).map { case (pos, sq) => pos -> sq.tile.get.letter }
              } else {
                (board.LettersLeft(pos).map { case (pos, sq) => pos -> sq.tile.get.letter } :+ pos -> let.letter) ::: board.LettersRight(pos).map { case (pos, sq) => pos -> sq.tile.get.letter }
              }
            (pos.x, pos.y, updatedList :+ otherWords)

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

                  val sq = if (horizontal) board.squareAt(Pos.posAt(pos.x, x).get) else Pos.posAt(x, pos.y).get

                  Pos.posAt(pos.x, x).get -> sq
              } :+ pos -> let.letter

              val newlist = x :+ pos -> let.letter
              val updatedList = newlist :: xs

              val otherWords: List[(Pos, Char)] =
                if (horizontal) {
                  (board.LettersAbove(pos).map { case (pos, sq) => pos -> sq.tile.get.letter } :+ pos -> let.letter) ::: board.LettersBelow(pos).map { case (pos, sq) => pos -> sq.tile.get.letter }
                } else {
                  (board.LettersLeft(pos).map { case (pos, sq) => pos -> sq.tile.get.letter } :+ pos -> let.letter) ::: board.LettersRight(pos).map { case (pos, sq) => pos -> sq.tile.get.letter }
                }
              (pos.x, pos.y, updatedList :+ otherWords)

            } else {
              return Right(MisPlacedLetters(pos.x, pos.y))
            }

          }

      }
        Left(lists._3)
    }

  }

}

object Main {
  def main(args: Array[String]) {
    val game = Game.init(List("jim", "joe"), Dictionary.load("C:\\workspace\\Scala\\scalascrabble\\src\\Dict\\en.txt"), LetterBag.init)

    val board = Board.init

    val newBrd = board.squares + (Pos.posAt(3, 2).get -> NormalSquare(Some(Letter('a', 1))))
    val testBoard = Board(newBrd)

    println(testBoard)

    val placed = List(
      Pos.posAt(3, 1).get -> Letter('A', 1),
      Pos.posAt(2, 1).get -> Letter('A', 1),
      Pos.posAt(1, 1).get -> Letter('A', 1))

    val blanks = List()

    val move = Move(Game(game.players, testBoard, game.playersMove, game.bag), placed, blanks)
    println(move.placedSorted)

    println(move.buildWords)
  }
}


