package scrabble;

/**
 * A square on the board. Extends ordered so that a list of squares may be sorted and the score evaluated in the
 *  right order. Might remove the ordered trait later as it's probably not required
 */
abstract class Square() extends Ordered[Square] {
  val tile: Option[Tile]
  val bonusString: String

  def compare(other: Square) = if (other.isInstanceOf[DoubleWordSquare] || other.isInstanceOf[TripleWordSquare]) -1 else 0

  def isEmpty = tile isEmpty

  def setLetter(letter: Tile): Square

  override def toString = tile.fold(bonusString){x=> x.toString()}
}

case class NormalSquare(tile: Option[Tile]) extends Square {
  val bonusString = "N"
  def setLetter(letter: Tile) = copy(tile = Some(letter))
}

case class DoubleLetterSquare(tile: Option[Tile]) extends Square {
  val bonusString = "DL"
  def setLetter(letter: Tile) = copy(tile = Some(letter))
}
case class TripleLetterSquare(tile: Option[Tile]) extends Square {
  val bonusString = "TL"
  def setLetter(letter: Tile) = copy(tile = Some(letter))
}

case class DoubleWordSquare(tile: Option[Tile]) extends Square {
  val bonusString = "DW"
  def setLetter(letter: Tile) = copy(tile = Some(letter))
}

case class TripleWordSquare(tile: Option[Tile] = None) extends Square {
  val bonusString = "TW"
  def setLetter(letter: Tile) = copy(tile = Some(letter))
}
