package scrabble;

/**
 * A square on the board. Extends ordered so that a list of squares may be sorted and the score evaluated in the
 *  right order. Might remove the ordered trait later as it's probably not required
 */
abstract class Square(tile: Option[Letter] = None) extends Ordered[Square] {
  def compare(other: Square) = if (other.isInstanceOf[DoubleWordSquare] || other.isInstanceOf[TripleWordSquare]) -1 else 0
}

case class NormalSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "N"
  }

}

case class DoubleLetterSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "DL"
  }

}
case class TripleLetterSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "TL"
  }
}

case class DoubleWordSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "DW"
  }

}

case class TripleWordSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "TW"
  }

}