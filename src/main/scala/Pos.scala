package scrabble

sealed case class Pos private (x: Int, y: Int, gridCordinates: String) {

  import Pos.posAt

  // Inspired by ScalaChess. A convenient way to access neighbouring squares.
  lazy val up: Option[Pos] = posAt(x, y + 1)
  lazy val down: Option[Pos] = posAt(x, y - 1)
  lazy val left: Option[Pos] = posAt(x - 1, y)
  lazy val right: Option[Pos] = posAt(x + 1, y)

}

object Pos {
  val min: Int = 1
  val max: Int = 15

  val startPosition: Pos = Pos(8, 8, "H8")

  /** Returns the position object at location (x,y) if it exists, else returns None (if it is out of the bounds of the board) */
  def posAt(x: Int, y: Int): Option[Pos] = allPositions get (x, y)

  // All the positions in the 15 x 15 board
  private val all: Stream[(Int, Int)] = for { i <- Stream.range(1, 16); j <- Stream.range(1, 16) } yield j -> i

  val allPositions: Map[(Int, Int), Pos] = {
    def gridCoords = Stream continually ('A' until 'P') flatten
    val mappedToCoords = gridCoords zip all
    mappedToCoords map { case (coord, (x, y)) => (x, y) -> Pos(x, y, coord.toString + y) } toMap
  }

}

	

