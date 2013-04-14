package scrabble

abstract class Tile() {
  val letter: Char
  val value: Int
}

case class Letter(letter: Char, value: Int) extends Tile {
  override def equals(that: Any) = {
    that match {
      case (Letter(chr, vl)) => (chr == letter)
      case _ => false
    }
  }
}

case class BlankLetter(letter: Char = '_') extends Tile {
  val value = 0
  
  override def equals(that: Any) = {
    that match {
      case (BlankLetter(chr)) => true
      case _ => false
    }
  }
}