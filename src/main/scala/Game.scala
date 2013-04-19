package scrabble;
case class Game private (
  players: Map[Int,Player],
  dictionary: Dictionary,
  board: Board,
  playersMove: Int, // Index into the list of players
  bag: LetterBag,
  moves: Int) {

  val currentPlayer = players(playersMove)
  val nextPlayerNo: Int = (playersMove + 1) % (players.size)
}

object Game {

  /**
   * Initialises a new game, with a list of player names, with a language specific dictionary and letter bag.
   *  There must be between 2 and 4 players. Returns None if this condition isn't met.
   */
  def make(playerNames: List[String], dictionary: Dictionary, letterbag: LetterBag): Option[Game] = {
    if (!(2 to 4 contains playerNames.size)) None else {
      // Distribute letters from the bag to the players
      val (players: List[(Int,Player)], remainingBag: LetterBag, player_no: Int) =

        playerNames.foldLeft((List.empty[(Int,Player)], letterbag, 0)) {
          case ((playerList, thebag, player_no), name) =>
            val (letters: List[Tile], bag) = thebag.remove(7)
            val player = Player(letters, name, 0)

            ((player_no -> player) :: playerList, bag, player_no + 1)
        }

      Some(Game(players.toMap, dictionary, Board.init, 0, remainingBag, 0))
    }

  }
}

