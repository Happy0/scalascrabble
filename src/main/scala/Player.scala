package scrabble

case class Player(
  letters: List[Option[Letter]],
  username: String,
  score: Int) {

}