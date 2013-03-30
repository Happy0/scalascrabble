package scrabble;

case class Game(
  players: List[Player],
  board: Board,
  playersMove: Int, // Index into the list of players
  bag: LetterBag) {

  require(players.size >= 2 && players.size <= 4)

  
}

