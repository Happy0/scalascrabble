package scrabble;

/**
 * A square on the board. Extends ordered so that a list of squares may be sorted and the score evaluated in the
 *  right order. Might remove the ordered trait later as it's probably not required
 */
abstract class Square() extends Ordered[Square] {
  val tile: Option[Letter]
  val bonusString: String

  def compare(other: Square) = if (other.isInstanceOf[DoubleWordSquare] || other.isInstanceOf[TripleWordSquare]) -1 else 0

  def isEmpty = tile.isEmpty

  def setLetter(letter: Letter): Square

  override def toString = tile match {
    case Some(x) => x.toString()
    case None => bonusString
  }

}

case class NormalSquare(tile: Option[Letter]) extends Square {
  val bonusString = "N"

  def setLetter(letter: Letter) = NormalSquare(Some(letter))

}

case class DoubleLetterSquare(tile: Option[Letter]) extends Square {
  val bonusString = "DL"

  def setLetter(letter: Letter) = DoubleLetterSquare(Some(letter))

}
case class TripleLetterSquare(tile: Option[Letter]) extends Square {
  val bonusString = "TL"

  def setLetter(letter: Letter) = TripleLetterSquare(Some(letter))
}

case class DoubleWordSquare(tile: Option[Letter]) extends Square {
  val bonusString = "DW"

  def setLetter(letter: Letter) = DoubleWordSquare(Some(letter))

}

case class TripleWordSquare(override val tile: Option[Letter] = None) extends Square {
  val bonusString = "TW"

  def setLetter(letter: Letter) = TripleWordSquare(Some(letter))

}
