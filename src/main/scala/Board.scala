package scrabble

import scala.List.range

case class Board(squares: Map[Pos, Square]) {

  override def toString = " " +
    range(Pos.min, Pos.max).map { x =>
      range(Pos.min, Pos.max).map { y =>
        Pos.at(x, y) flatMap { pos =>
          val squareStr = this.squares.get(pos)
          squareStr map { square =>
              if (y == 15) square + "\n " else square.tile.fold(square + "|") { _ => s"[$square]" }
          }
        }
      }.flatten.mkString
    }.mkString

  def squareAt(pos: Pos): Option[Square] = squares.get(pos)

  def tileAt(pos: Pos) = squares.get(pos).flatMap(_.tile)

  def lettersAbove(pos: Pos): List[PosSquare] = walkTiles(pos, pos => pos.up)
  def lettersBelow(pos: Pos): List[PosSquare] = walkTiles(pos, pos => pos.down).reverse
  def lettersLeft(pos: Pos): List[PosSquare] = walkTiles(pos, pos => pos.left).reverse
  def lettersRight(pos: Pos): List[PosSquare] = walkTiles(pos, pos => pos.right)

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

  def placeLetter(pos: Pos, let: Tile): Option[Board] =
    squareAt(pos).fold[Option[Board]](None)(sq => Some(copy(squares = squares.updated(pos, sq.setLetter(let)))))

}

object Board {

  def apply(squares: Traversable[(Pos, Square)]): Board = Board(squares.toMap)

  def apply(): Board = {
    // Return a list of all the bonus squares. The rest of the board are then 'normal squares'
    val bonusSquares: Map[(Int, Int), Square] = {
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
      val offsets: List[(Int => Int, Int => Int)] = List((x => Pos.max - x, y => y),
        (x => x, y => Pos.max - y), (x => Pos.max - x, y => Pos.max - y))

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

    val board = Pos.allPositionsMap map {
      case ((x, y), pos) =>
        val special = bonusSquares.get(x, y)
        val square: Square = special getOrElse NormalSquare(None)
        pos -> square
    }

    Board(board)
  }

}



