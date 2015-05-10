package scrabble;

class GameTest extends ScrabbleTest {

  "a game should" should {

    "have no less than 2 players" in {
      Game(List("jim"), Dictionary("Dict/en.txt"), LetterBag()) must beNone
    }

    "have no more than 4 players" in {
      Game(List("jim", "joe", "charlie", "noam", "test"),
        Dictionary("Dict/en.txt"), LetterBag()) must beNone
    }

    "Initialise a game properly" in {
      game.foreach(_ => eachGame(_))

      def eachGame(g: Game) = {
        g.bag.size must beEqualTo(100 - 14) // Distribute the initial tiles

        g.players must haveKeys(0, 1)

        g.players(0).letters must have size 7
        g.players(1).letters must have size 7

        g.board must beEqualTo(Board())

        g.playersMove must beEqualTo(0)

        g.moves must beEqualTo(0)

        g.consecutivePasses must beEqualTo(0)
      }

    }

    def setPlayer(game: Game, mv: Int) = game.copy(playersMove = mv)

    "keep track of the next player" in {

      game foreach {
        game =>
          (0 to 1).foreach { i =>
            val gm = setPlayer(game, i)
            if (i == 0) gm.nextPlayerNo must beEqualTo(1) else gm.nextPlayerNo must beEqualTo(0)
          }

      }

      val fourPlayerGame = Game(List("jim", "joe", "charlie", "noam"), Dictionary("Dict/en.txt"), LetterBag())
      fourPlayerGame must beSome

      fourPlayerGame foreach {
        fourPlayerGame =>
          (0 to 3).foreach { i =>
            val gm = setPlayer(fourPlayerGame, i)
            if (i == 3) gm.nextPlayerNo must beEqualTo(0) else gm.nextPlayerNo must beEqualTo(i + 1)
          }
      }

    }
  }

}