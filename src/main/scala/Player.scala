package scrabble

case class Player(
  letters: List[Letter],
  username: String,
  score: Int) {
  
  def replaceLetters(lettrs: List[Letter]) : Player = copy(letters = lettrs)
  
}

