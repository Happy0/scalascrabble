package scrabble;

class Game(
  players: Map[String, Player],
  board: Board,
  playerMove: String) {

  require(players.size >= 2 && players.size <= 4)

}