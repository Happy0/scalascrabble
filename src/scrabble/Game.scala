package scrabble;

case class Game(
  players: List[Player],
  board: Board,
  playersMove: Int, // Index into the list of players
  bag: LetterBag) {

  require(players.size >= 2 && players.size <= 4)
  
  /** Produces a new game state from a move */ 
  def apply(move: Move) : Game = {}
  
}

object Game {
  
  /** A new game, with a language specific dictionary and letter bag  */
  def apply(players: List[Player], dictionary: Dictionary, letterbag: LetterBag): Game = Game(players, Board.init, 0, letterbag)
}

