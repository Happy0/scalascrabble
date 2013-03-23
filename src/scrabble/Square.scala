package scrabble;

abstract class Square(tile: Option[Tile] = None) {
  
  
}
  
case class NormalSquare(tile: Option[Tile] = None) extends Square(tile) {
  override def toString = tile match {
    case Some(x) => x.toString()
    case None => "N"
  }
}

case class DoubleLetterSquare(tile: Option[Tile] = None)  extends Square(tile) {
    override def toString = tile match {
    case Some(x) => x.toString()
    case None => "DL"
  }
}
  
case class TripleLetterSquare(tile: Option[Tile] = None)  extends Square(tile) {
    override def toString = tile match {
    case Some(x) => x.toString()
    case None => "TL"
  }
}
  
case class DoubleWordSquare(tile: Option[Tile] = None)  extends Square(tile) {
    override def toString = tile match {
    case Some(x) => x.toString()
    case None => "DW"
  }
}
  
case class TripleWordSquare(tile: Option[Tile] = None)  extends Square(tile) {
    override def toString = tile match {
    case Some(x) => x.toString()
    case None => "TW"
  }
}