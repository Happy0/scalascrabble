package scrabble;

case class Game(
  players: Map[String, Player],
  board: Board,
  playersMove: String,
  bag: LetterBag) {

  require(players.size >= 2 && players.size <= 4)

  //def apply(move: Move) : Game = {}
  
}

