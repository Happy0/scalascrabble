package scrabble;

case class Board(
  squares: Map[Pos, Square]) {

  override def toString = {
    List.range(1, 16).map { (x) =>
      List.range(1, 16).map {
        y =>
          val squareStr = this.squares.get(Pos.posAt(y, x).get).get.toString + " "
          if (y == 15) squareStr + "\n " else squareStr
      }.mkString
    }.mkString
  }

  // *shakes fist at Ornicar and the compiler*
  def squareAt(pos: Pos): Option[Square] = squares get pos

  def tileAt(pos: Pos) = squares get pos flatMap (_.tile)

  def LettersAbove(pos: Pos): List[PosSquare] = walkTiles(pos, pos => pos.up)
  def LettersBelow(pos: Pos): List[PosSquare] = walkTiles(pos, pos => pos.down) reverse
  def LettersLeft(pos: Pos): List[PosSquare] = walkTiles(pos, pos => pos.left) reverse
  def LettersRight(pos: Pos): List[PosSquare] = walkTiles(pos, pos => pos.right)

  private def walkTiles(from: Pos, to: Direction): PosSquares =
    to(from) ?? { pos =>
      squareAt(pos) ?? {
        sq =>
          tileAt(pos) ?? {
            tile =>
              (pos, sq, tile) :: walkTiles(pos, to)
          }

      }
    }

  //  squareAt(pos) map {sq => copy(squares = squares.updated(pos, sq.setLetter(let))) }
  def placeLetter(pos: Pos, let: Tile): Option[Board] =
    squareAt(pos).fold[Option[Board]](None)(sq => Some(copy(squares = squares.updated(pos, sq.setLetter(let)))))

}

object Board {
  import Pos._

  def apply(squares: Traversable[(Pos, Square)]): Board = Board(squares.toMap)

  val init: Board =
    {
      // Return a list of all the bonus squares. The rest of the board are then 'normal squares'
      val bonusSquares: Map[(Int, Int), Square] =
        {
          val leftQuarter: List[((Int, Int), Square)] =
            List(
              (1, 1) -> TripleWordSquare(None),
              (4, 1) -> DoubleLetterSquare(None),
              (8, 1) -> TripleWordSquare(None),
              (2, 2) -> DoubleWordSquare(None),
              (6, 2) -> TripleLetterSquare(None),
              (3, 3) -> DoubleWordSquare(None),
              (7, 3) -> DoubleLetterSquare(None),
              (1, 4) -> DoubleLetterSquare(None),
              (4, 4) -> DoubleWordSquare(None),
              (8, 4) -> DoubleLetterSquare(None),
              (8, 8) -> DoubleWordSquare(None),
              (5, 5) -> DoubleWordSquare(None),
              (2, 6) -> TripleLetterSquare(None),
              (6, 6) -> TripleLetterSquare(None),
              (3, 7) -> DoubleLetterSquare(None),
              (7, 7) -> DoubleLetterSquare(None),
              (1, 8) -> TripleWordSquare(None),
              (4, 8) -> DoubleLetterSquare(None))

          /* List of functions to produce the x and y offsets for the other quarters of the board,
           *  using the symmetry of the board */
          val offsets: List[(Int => Int, Int => Int)] = List((x => Pos.max + 1 - x, y => y),
            (x => x, y => Pos.max + 1 - y), (x => Pos.max + 1 - x, y => Pos.max + 1 - y))

          // Produce and return the other quarters of special squares, accumulating the map
          val wholeBoard = offsets.foldLeft(leftQuarter.toMap) {
            case (map, (f, g)) =>
              val inMap = leftQuarter.foldLeft(map) {

                case (inner, ((x, y), square)) =>
                  val res = ((f(x), g(y)), square)
                  inner + res

              }
              inMap
          }

          wholeBoard
        }

      // Construct and return the board. Anything that is not a bonus square is a NormalSquare.
      val all = Pos.all

      val board = all.foldLeft(Map.empty[Pos, Square]) {
        case (map, (x, y)) =>
          val special = bonusSquares.get(x, y)
          val square: Square = special getOrElse NormalSquare(None)
          val entry = Pos.posAt(x, y).get -> square

          map + entry
      }
      Board(board)
    }

}



