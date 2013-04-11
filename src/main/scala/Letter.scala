package scrabble

abstract class Tile() {
  val letter: Char
  val value: Int

}

case class Letter(letter: Char, value: Int) extends Tile

case class BlankLetter(letter: Char) extends Tile {
  val value = 0
}