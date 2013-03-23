package scrabble;

abstract class Square(tile: Option[Tile] = None)
  
case class NormalSquare(tile: Option[Tile] = None) extends Square(tile)

case class DoubleLetterSquare(tile: Option[Tile] = None)  extends Square(tile)
  
case class TripleLetterSquare(tile: Option[Tile] = None)  extends Square(tile)
  
case class DoubleWordSquare(tile: Option[Tile] = None)  extends Square(tile)
  
case class TripleWordSquare(tile: Option[Tile] = None)  extends Square(tile)