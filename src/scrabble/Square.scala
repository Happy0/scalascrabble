package scrabble;

/**
 * A square on the board. Extends ordered so that a list of squares may be sorted and the score evaluated in the
 *  right order. Might remove the ordered trait later as it's probably not required
 */
abstract class Square() extends Ordered[Square] {
  val tile: Option[Letter]

  def compare(other: Square) = if (other.isInstanceOf[DoubleWordSquare] || other.isInstanceOf[TripleWordSquare]) -1 else 0

  def isEmpty = tile.isEmpty
}

case class NormalSquare(tile: Option[Letter]) extends Square {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "N"
  }

}

case class DoubleLetterSquare(tile: Option[Letter]) extends Square {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "DL"
  }

}
case class TripleLetterSquare(tile: Option[Letter]) extends Square {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "TL"
  }

  def addToWordScore(soFar: Int): Int = soFar + tile.get.value * 3
}

case class DoubleWordSquare(tile: Option[Letter]) extends Square {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "DW"
  }

}

case class TripleWordSquare(override val tile: Option[Letter] = None) extends Square {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "TW"
  }

}