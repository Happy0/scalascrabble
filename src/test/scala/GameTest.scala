package scrabble;

class GameTest extends ScrabbleTest {

  "a game should" should {

    "have no less than 2 players" in {
      val game = Game.make(List("jim"), Dictionary.load("Dict/en.txt"),
        LetterBag.init) must beNone
    }

    "have no more than 4 players" in {
      val game = Game.make(List("jim", "joe", "charlie", "noam", "test"),
        Dictionary.load("Dict/en.txt"), LetterBag.init) must beNone
    }

    "Initialise a game properly" in {
      game.bag.size must beEqualTo(100 - 14) // Distribute the initial tiles

      game.players must haveKeys(0, 1)

      game.players(0).letters must have size 7
      game.players(1).letters must have size 7

      game.board must beEqualTo(Board.init)

      game.playersMove must beEqualTo(0)

      game.moves must beEqualTo(0)

      game.consecutivePasses must beEqualTo(0)
    }

    def setPlayer(game: Game, mv: Int) = game.copy(playersMove = mv)

    "keep track of the next player" in {
      (0 to 1).foreach { i =>
        val gm = setPlayer(game, i)
        if (i == 0) gm.nextPlayerNo must beEqualTo(1) else gm.nextPlayerNo must beEqualTo(0)
      }

      val fourPlayerGame = Game.make(List("jim", "joe", "charlie", "noam"),  Dictionary.load("Dict/en.txt"), LetterBag.init).get
      
      (0 to 3).foreach { i =>
        val gm = setPlayer(fourPlayerGame, i)
        if (i == 3) gm.nextPlayerNo must beEqualTo(0) else gm.nextPlayerNo must beEqualTo(i + 1)
      }
    }
  }

}