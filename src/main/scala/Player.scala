package scrabble

case class Player(
  letters: List[Tile],
  username: String,
  score: Int) {
  
  def replaceLetters(lettrs: List[Tile]) : Player = copy(letters = lettrs)
  
}

