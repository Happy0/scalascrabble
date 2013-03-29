package scrabble;

/** A square on the board. Extends ordered so that a list of squares may be sorted and the score evaluated in the
 *  right order*/
abstract class Square(tile: Option[Letter] = None) extends Ordered[Square]

case class NormalSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "N"
  }

  def compare(other: Square) = if (other.isInstanceOf[NormalSquare]) 0 else -1
}

case class DoubleLetterSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "DL"
  }

  def compare(other: Square) = {
    if (other.isInstanceOf[NormalSquare]) 1
    else if (!other.isInstanceOf[DoubleWordSquare] && !other.isInstanceOf[TripleWordSquare]) -1 else 0
  }

}
case class TripleLetterSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "TL"
  }

  def compare(other: Square) = other.compare(DoubleLetterSquare(None))
}

case class DoubleWordSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "DW"
  }

  def compare(other: Square) = if (other.compare(TripleLetterSquare(None)) <= 0) 1 else 0

}

case class TripleWordSquare(tile: Option[Letter] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "TW"
  }

  def compare(other: Square) = if (other.compare(TripleLetterSquare(None)) <= 0) 1 else 0

}