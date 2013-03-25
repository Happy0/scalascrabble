package scrabble;

case class Board(
  squares: Map[Pos, Square]) {

  override def toString = {
    List.range(1, 16).map { (x) =>
      List.range(1, 16).map {
        y =>
          val squareStr = this.squares.get(Pos.posAt(x, y).get).get.toString
          if (y == 15) squareStr + "\n " else squareStr
      }
    }.toString
  }
}

object Board {
  import Pos._

  def apply(squares: Traversable[(Pos, Square)]): Board = Board(squares.toMap)

  def init: Board =
    {
      // Return a list of all the bonus squares. The rest of the board are then 'normal squares'
      val bonusSquares: Map[(Int, Int), Square] =
        {

          /* Quarter the board, then find the values from the rest of the board using the symmetry. I hope I can maths ;x..*/
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
              (5, 5) -> DoubleWordSquare(None),
              (2, 6) -> TripleLetterSquare(None),
              (6, 6) -> TripleLetterSquare(None),
              (3, 7) -> DoubleLetterSquare(None),
              (7, 7) -> DoubleLetterSquare(None),
              (1, 8) -> TripleWordSquare(None),
              (4, 8) -> DoubleLetterSquare(None))

          // List of functions to produce the offsets for the other quarters of the board
          val offsets: List[(Int => Int, Int => Int)] = List((x => Pos.max + 1 - x, y => y), (x => x, y => Pos.max + 1 - y), (x => Pos.max + 1 - x, y => Pos.max + 1 - y))

          // Produce the other quarters of special squares
          offsets.map {
            case (f, g) =>
              leftQuarter.map { case ((x, y), square) => ((f(x), g(y)), square) }
          }.foldLeft(leftQuarter)(((x, y) => x ++ y)).toMap
        }

      // Construct the board. Anything that is not a bonus square is a NormalSquare
      val all = Pos.all
      val list: List[(Pos, Square)] = for {
        a <- all
        (x, y) = a
        special = bonusSquares.get(x, y)

        square: Square = special match {
          case None => NormalSquare(None)
          case Some(x) => x
        }

      } yield Pos.posAt(x, y).get -> square

      Board(list.toMap)
    }

  def main(args: Array[String]) {
    val board = Board.init
    println(board)

  }

}



