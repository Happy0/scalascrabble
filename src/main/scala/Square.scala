package scrabble;

/**
 * A square on the board. Extends ordered so that a list of squares may be sorted and the score evaluated in the
 *  right order. Might remove the ordered trait later as it's probably not required
 */
abstract class Square() extends Ordered[Square] {
  val tile: Option[Tile]
  val bonusString: String

  def compare(other: Square) = if (other.isInstanceOf[DoubleWordSquare] || other.isInstanceOf[TripleWordSquare]) -1 else 0

  def isEmpty = tile.isEmpty

  def setLetter(letter: Tile): Square

  override def toString = tile match {
    case Some(x) => x.toString()
    case None => bonusString
  }

}

case class NormalSquare(tile: Option[Tile]) extends Square {
  val bonusString = "N"

  def setLetter(letter: Tile) = NormalSquare(Some(letter))

}

case class DoubleLetterSquare(tile: Option[Tile]) extends Square {
  val bonusString = "DL"

  def setLetter(letter: Tile) = DoubleLetterSquare(Some(letter))

}
case class TripleLetterSquare(tile: Option[Tile]) extends Square {
  val bonusString = "TL"

  def setLetter(letter: Tile) = TripleLetterSquare(Some(letter))
}

case class DoubleWordSquare(tile: Option[Tile]) extends Square {
  val bonusString = "DW"

  def setLetter(letter: Tile) = DoubleWordSquare(Some(letter))

}

case class TripleWordSquare(override val tile: Option[Tile] = None) extends Square {
  val bonusString = "TW"

  def setLetter(letter: Tile) = TripleWordSquare(Some(letter))

}
