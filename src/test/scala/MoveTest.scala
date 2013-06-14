package scrabble

import scala.util.{ Try, Success, Failure }
import scala.util.Failure
import scalaz.NonEmptyList
import org.specs2.matcher.MatchResult

class MoveTest extends ScrabbleTest {

  def withGameAndPositions(game: Option[Game], placed: Option[NonEmptyList[(Pos, Tile)]])(
    behaviour: ValidPlaceLettersMove => Unit) = {
    game must beSome
    placed must beSome

    placed foreach {
      place =>
        game foreach {
          game =>
            val valid = PlaceLettersMove(game, place).validate
            valid.toOption must not beNone

            PlaceLettersMove(game, place).validate foreach behaviour
        }

    }
  }

  def builtToStr(lists: List[List[(Pos, Square, Tile)]]): List[String] = lists.map { list =>
    list.map { case (pos, sq, letter) => letter.letter }.mkString
  }

  val playedGame = game.flatMap { game =>
    crossedWords.map {
      words =>
        game.copy(board = words, moves = 1)
    }

  }

  def furtherGame(game: Option[Game], place: Option[NonEmptyList[PosTile]]): Option[Game] = {
    game flatMap {
      game =>
        place flatMap {
          place =>
            PlaceLettersMove(game, place).validate flatMap (_.makeMove) toOption

        }
    }
  }

  val gibberishPlace = {
    addPlaceLists(toPlace("T", false, pos(6, 4)), toPlace("ESTS", false, pos(6, 6)))
  }

  val blankGame: Option[Game] = {
    val str = "JEARVINENVO_NILLEWBKONUIEUWEAZBDESIAPAEOOURGOCDSNIADOAACAR_RMYELTUTYTEREOSITNIRFGPHAQLHESOIITXFDMETG"
    val bag = LetterBag.fromLetters(str, letterBag.tileSet)

    bag flatMap {
      bag =>
        Game.make(List("a", "b", "c", "d"), enDict, bag)
    }

  }

  val ravinePlaced = {
    blankGame flatMap {
      game =>
        toPlace("ravine", true, pos(8, 8)) flatMap {
          place =>
            PlaceLettersMove(game, place).validate flatMap (_.makeMove) toOption
        }
    }

  }

  val coversTwoBonuses: Option[ValidPlaceLettersMove] = {

    val place = addPlaceLists(toPlace("ven", false, pos(11, 5)), toPlace(
      "son", false, pos(11, 9)))

    val placed = pos(11, 9) flatMap {
      pos =>
        safeUpdateTile(place, 3, BlankLetter('S'))
    }

    placed flatMap {
      placed =>
        ravinePlaced flatMap {
          game1 =>
            PlaceLettersMove(game1, placed).validate.toOption
        }
    }

  }
  coversTwoBonuses must not beNone

  val modifiedPlayer = playedGame flatMap (_.currentPlayer.map(_.replaceLetters(toLetters("tory"))))

  val modifiedGame = {
    modifiedPlayer flatMap {
      player => playedGame map (g => g.copy(players = g.players.updated(g.playersMove, player)))

    }
  }

  def checkBuiltWords(game: Option[Game], place: Option[NonEmptyList[PosTile]], shouldContain: List[String]) = {
    withGameAndPositions(game, place) {
      move =>
        move.formedWords.toOption must not beNone

        move.formedWords foreach {
          built =>
            val words = builtToStr(built)
            words must containAllOf(shouldContain)
            words must have size shouldContain.size
        }
    }
  }

  "a move should" should {

    "fail if the first move does not intersect with the star square" in {
      val place = toPlace("test", true, pos(11, 2))

      withGameAndPositions(game, place)(_.makeMove must beEqualTo(Failure(FirstMovePositionWrong(0))))
    }

    "not place letters on top of occupied squares" in {
      val place = toPlace("hello", true, pos(3, 5))

      withGameAndPositions(playedGame, place)(_.makeMove must beEqualTo(Failure(SquareOccupiedClientError(6))))
    }

    "not place letters that the player does not have" in {
      val place = toPlace("tone", true, pos(8, 3))

      withGameAndPositions(modifiedGame, place)(_.makeMove must beEqualTo(Failure(playerDoesNotHaveLettersClientError(7))))
    }

    "fail if the word is not attached to an existing word" in {
      val place = toPlace("test", true, pos(1, 1))

      withGameAndPositions(playedGame, place)(_.makeMove must beEqualTo(Failure(NotAttachedToWord(2))))
    }

    "build multiple words from letters placed adjacent to other squares from horizontally placed letters" in {
      val place = addPlaceLists(toPlace("o", true, pos(6, 6)), toPlace("e", true, pos(8, 6)))
      checkBuiltWords(playedGame, place, List("TO", "ORE", "RE"))
    }

    "build multiple words from letters placed adjacent to other squares from vertically placed letters " in {
      checkBuiltWords(playedGame, gibberishPlace, List("TTESTS", "TC", "ER", "SE", "TS"))
    }

    "extend an existing word horizontally" in {
      val place = toPlace("tares", true, pos(8, 8))
      checkBuiltWords(playedGame, place, "STARES" :: Nil)

    }

    "extend an existing word vertically" in {
      val place = toPlace("sdf", false, pos(7, 9))
      checkBuiltWords(playedGame, place, "SCORESSDF" :: Nil)
    }

    "extend an existing word on the left and right" in {
      val place = addPlaceLists(toPlace("SM", true, pos(1, 5)), toPlace("S", true, pos(10, 5)))
      checkBuiltWords(playedGame, place, "SMHISTORYS" :: Nil)
    }

    "extend an existing word above and below" in {
      val place = addPlaceLists(toPlace("SM", false, pos(7, 1)), toPlace("ST", false, pos(7, 9)))
      checkBuiltWords(playedGame, place, "SMSCORESST" :: Nil)
    }

    def checkMisplaced(game: Option[Game], place: Option[NonEmptyList[PosTile]], errorAt: (Int, Int)) = {
      withGameAndPositions(game, place) {
        move =>
          move.formedWords must beEqualTo(Failure(MisPlacedLetters(errorAt._1, errorAt._2)))
      }
    }

    "warn about misplaced letters" in {

      val place = toPlace("test", true, pos(1, 1))
      val first = safeUpdateTile(place, 1, Pos.posAt(3, 2), 'C')

      val place2 = toPlace("test", true, pos(1, 1))
      val second = safeUpdateTile(place2, 3, pos(5, 1), 'C')

      val toPlace3 = addPlaceLists(toPlace("T", true, pos(2, 5)), toPlace("fd", true, pos(11, 5)))

      // Square placed outside the 'line' (i.e above)
      checkMisplaced(playedGame, first, (3, 1))

      // linear, but missing a square to complete the the 'line'
      checkMisplaced(playedGame, second, (5, 1))

      // Start to complete a word at one side, but misplace letter at the other
      checkMisplaced(playedGame, toPlace3, (11, 5))

    }

    "reject invalid words" in {
      withGameAndPositions(playedGame, gibberishPlace) {
        move =>
          val words = "TTESTS" :: "TC" :: "SE" :: "TS" :: Nil

          move.makeMove.get must throwA[WordsNotInDictionary].like {
            case e: WordsNotInDictionary =>
              e.words must have size 4 // 'RE' should be the only valid word
              e.words must containAllOf(words)
          }

      }

    }

    def checkScore(game: Option[Game], place: Option[NonEmptyList[PosTile]], totalScore: Int) = {
      withGameAndPositions(game, place) {
        move =>
          val score = move.score
          score.toOption must not beNone

          score.foreach {
            score => score.overAllScore must beEqualTo(totalScore)
          }
      }
    }

    "calculate scores correctly" in {

      //  normal
      val placeNormal = addPlaceLists(toPlace("wa", false, pos(5, 3)), toPlace("p", false, pos(5, 6)))
      checkScore(playedGame, placeNormal, 9)

      // double letter
      val placeDoubleLetter = toPlace("tyle", true, pos(8, 3))
      checkScore(playedGame, placeDoubleLetter, 12)

      // Double word
      val doublePlace = toPlace("stair", true, pos(2, 3))
      checkScore(playedGame, doublePlace, 12)

      // triple letter
      val tripleLetterPlace = toPlace("ale", true, pos(9, 6))
      checkScore(playedGame, tripleLetterPlace, 10)

      // triple word
      val tripleWordPlace = toPlace("TAO", false, pos(8, 1))
      checkScore(playedGame, tripleWordPlace, 11)

      // Multiple words

      //@TODO: refactor
      val playedFurther = game flatMap {
        game =>
          val place = addPlaceLists(toPlace("wa", false, pos(5, 3)), toPlace("p", false, pos(5, 6)))
          crossedWords flatMap {
            words =>
              placeSquares(words, place) map {
                ohgodthemeanderingmapsareover =>
                  game.copy(board = ohgodthemeanderingmapsareover)
              }

          }

      }

      // val playedFurther = furtherGame(playedGame, addPlaceLists(toPlace("wa", false, pos(5, 3)), toPlace("p", false, pos(5, 6))))

      val multipleWordPlace = toPlace("YA", false, pos(6, 2))
      withGameAndPositions(playedFurther, multipleWordPlace) {
        move =>
          val score = move.score
          score.toOption must not beNone

          score foreach {
            score =>
              score.overAllScore must beEqualTo(19)
              score.individualScores must contain("YA" -> 13)
              score.individualScores must contain("WAS" -> 6)
          }
      }

      // Covering multiple bonus squares
      coversTwoBonuses foreach { move =>
        val score = move.score.toOption
        score.toOption must not beNone

        score foreach (_.overAllScore must beEqualTo(36))
      }

    }

    "place one letter" in {
      val game1 = furtherGame(blankGame, toPlace("ravine", true, pos(8, 8)))
      val place = toPlace("O", true, pos(8, 7))

      checkBuiltWords(game1, place, "OR" :: Nil)

      // @TODO: Horizontal single letter

      // @TODO: Build multiple words
    }

    "handle blank letters" in {
      // ENVO_NIL

      coversTwoBonuses foreach {
        move =>
          val words = move.formedWords
          words.toOption must beSome

          words foreach {
            wrds =>
              builtToStr(wrds) must contain("VENISON")
          }
      }

    }

    val predictableGame = {
      val place = toPlace("lurid", true, pos(8, 8))
      furtherGame(predictableLetterbagGame, place)
    }
    predictableGame must beSome

    "replace the letters that a player played" in {
      predictableGame foreach {
        predictableGame =>
          val player = predictableGame.players.get(0)
          player must beSome

          player foreach {
            player =>
              player.letters must containAllOf(toLetters("SV"))
              player.letters must containAllOf(toLetters("EIYUR"))
          }

      }

    }

    "transition the game state correctly" in {
      predictableGame foreach {
        predictableGame =>
          predictableGame.moves must beEqualTo(1)
          pos(7, 8) foreach {
            pos =>
              predictableGame.board.LettersRight(pos).map { case (pos, sq, let) => let.letter }.mkString must beEqualTo("LURID")
          }

          predictableGame.bag.lettersAsString must beEqualTo(
            "ADYEICBLEDHMSIXNFERAIWOANETGAELGFIUT_TJHAI_BDONENOECTRIEEREKOAZPVETONSASURAPMNOTO")

          predictableGame.currentPlayer must beSome
          predictableGame.currentPlayer foreach { _.letters.map(_.letter).mkString must beEqualTo("IGQAWLO") }
          predictableGame.playersMove must beEqualTo(1)
      }

    }

    "ends the game in the appropriate conditions" in {

    }

    "handle exchange moves correctly" in {

      predictableGame foreach {
        game =>
          val move = ExchangeMove(game, toLetters("IGQ"))
          val moveMade = move.makeMove

          moveMade must not be equalTo(Failure(PlayerDoesNotHaveLettersToExchange()))
          moveMade must not be equalTo(Failure(MustExchangeSameNumberofLetters()))
          moveMade.toOption must not beNone

          move.makeMove foreach {
            newGame =>
              val player = newGame.players get (game.playersMove)
              player must not beNone

              player foreach {
                player =>
                  val test = player.letters map (c => c.letter) mkString

                  test must beEqualTo("ADYAWLO")

              }
              newGame.moves must beEqualTo(game.moves + 1)
              newGame.bag.letters.intersect(toLetters("IGQ") ::: game.bag.letters.drop(3)).size must beEqualTo(newGame.bag.size)

          }
      }

    }

    "handle pass moves correctly" in {

    }

  }

}
