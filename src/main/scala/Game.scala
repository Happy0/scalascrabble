package scrabble;

case class Game(
  players: List[Player],
  dictionary: Dictionary,
  board: Board,
  playersMove: Int, // Index into the list of players
  bag: LetterBag,
  moves: Int) {

  require(players.size >= 2 && players.size <= 4)

  val currentPlayer = players(playersMove)

}

object Game {

  /** Initialises a new game, with a list of player names, with a language specific dictionary and letter bag  */
  def init(playerNames: List[String], dictionary: Dictionary, letterbag: LetterBag): Game = {

    // Distribute letters from the bag to the players
    val (players: List[Player], remainingBag: LetterBag) =

      playerNames.foldLeft((List.empty[Player], letterbag)) {
        case ((playerList, thebag), name) =>
          val (letters: List[Letter], bag) = thebag.remove(7)
          val player = Player(letters.map(x => Some(x)), name, 0)

          ((player :: playerList), bag)
      }

    Game(players, dictionary, Board.init, 0, remainingBag,0)
  }
}
