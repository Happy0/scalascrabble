package scrabble

sealed case class Pos(x: Int, y: Int, gridCoordinates: String) {

  import Pos.at

  lazy val up: Option[Pos] = at(x, y + 1)
  lazy val down: Option[Pos] = at(x, y - 1)
  lazy val left: Option[Pos] = at(x - 1, y)
  lazy val right: Option[Pos] = at(x + 1, y)

}

object Pos {
  val min: Int = 1
  val max: Int = 16

  val startPosition: Pos = Pos(8, 8, "H8")

  /** Returns the position object at location (x,y) if it exists, else returns None (if it is out of the bounds of the board) */
  def at(x: Int, y: Int): Option[Pos] = allPositionsMap.get(x, y)

  // All the positions in the 15 x 15 board
  private val all: Stream[(Int, Int)] = for { i <- Stream.range(min, max); j <- Stream.range(min, max) } yield j -> i

  val allPositionsMap: Map[(Int, Int), Pos] = {
    def gridCoords = Stream.continually('A'.until('P')).flatten
    val mappedToCoords = all.zip(gridCoords)
    mappedToCoords map { case ((x, y), coord) => (x, y) -> Pos(x, y, coord.toString + y) } toMap
  }

  val allPositions: Iterable[Pos] = allPositionsMap map {
    case ((x, y), pos) => pos
  }

  val allOrderedPositions: List[Pos] = {
    List.range(Pos.min, Pos.max).map { x =>
      List.range(Pos.min, Pos.max).flatMap { y =>
        Pos.at(y, x)
      }
    }.flatten
  }

}
