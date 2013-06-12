package scrabble

import scala.util.{ Try, Success, Failure }
import scala.util.Failure
import scalaz.NonEmptyList
import org.specs2.matcher.MatchResult

import org.specs2.matcher.TraversableMatchers

class MoveTest extends ScrabbleTest {

  val playedGame = game.flatMap { game =>
    crossedWords.map {
      words =>
        game.copy(board = words, moves = 1)
    }

  }

  val gibberishWordsMove: Option[ValidPlaceLettersMove] = {
    playedGame flatMap {
      playedGame =>
        val place = toPlace("T", false, pos(6, 4)) flatMap { a =>
          toPlace("ESTS", false, pos(6, 6)) map { b =>
            a ++ b
          }
        }

        place flatMap {
          place =>
            PlaceLettersMove(playedGame, place).validate.toOption
        }
    }

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

    val placed = place flatMap {
      placed =>
        pos(11, 9) map {
          pos =>
            placed.updated(3, pos -> BlankLetter('S'))

        }
    }

    placed flatMap {
      placed =>
        ravinePlaced flatMap {
          game1 =>
            PlaceLettersMove(game1, placed).validate.toOption
        }
    }

  }

  val modifiedPlayer = playedGame flatMap (_.currentPlayer.map(_.replaceLetters(toLetters("tory"))))

  val modifiedGame = {
    modifiedPlayer flatMap {
      player => playedGame map (g => g.copy(players = g.players.updated(g.playersMove, player)))

    }
  }

  "a move should" should {

    "fail if the first move does not intersect with the star square" in {
      val place = toPlace("test", true, pos(11, 2))

      place flatMap {
        place =>
          game map {
            game =>
              PlaceLettersMove(game, place).validate flatMap (_.makeMove) must beEqualTo(Failure(FirstMovePositionWrong(0)))

          }
      } should beSome

    }

    "not place letters on top of occupied squares" in {
      playedGame flatMap {
        game =>
          toPlace("hello", true, pos(3, 5)) map {
            place =>
              PlaceLettersMove(game,
                place).validate.flatMap(_.makeMove) must beEqualTo(Failure(SquareOccupiedClientError(6)))

          }

      } must beSome

    }

    "not place letters that the player does not have" in {
      val place = toPlace("tone", true, pos(8, 3))

      place.flatMap {
        place =>
          modifiedGame map {
            game =>
              PlaceLettersMove(game,
                place).validate flatMap (_.makeMove) must beEqualTo(Failure(playerDoesNotHaveLettersClientError(7)))

          }
      } must beSome

    }

    "fail if the word is not attached to an existing word" in {
      val place = toPlace("test", true, pos(1, 1))

      place flatMap {
        place =>
          playedGame map {
            game =>
              PlaceLettersMove(game, place).validate.flatMap(_.makeMove) must beEqualTo(Failure(NotAttachedToWord(2)))

          }
      } must beSome

    }

    def withGameAndPositions(game: Option[Game], placed: Option[NonEmptyList[(Pos, Tile)]])(
      behaviour: ValidPlaceLettersMove => Try[MatchResult[Any]]) = {
      placed flatMap {
        place =>
          game map {
            game =>
              PlaceLettersMove(game, place).validate map behaviour
          }

      } must beSome
    }

    def builtToStr(lists: List[List[(Pos, Square, Tile)]]): List[String] = lists.map { list =>
      list.map { case (pos, sq, letter) => letter.letter }.mkString
    }

    "build multiple words from letters placed adjacent to other squares from horizontally placed letters" in {
      val place = addPlaceLists(toPlace("o", true, pos(6, 6)), toPlace("e", true, pos(8, 6)))

      // val mv = PlaceLettersMove(playedGame, place).validate.get

      withGameAndPositions(playedGame, place) {
        move =>

          move.formedWords map {
            built: List[List[(scrabble.Pos, scrabble.Square, scrabble.Tile)]] =>

              val words = builtToStr(built)

              words must contain("TO")
              words must contain("ORE")
              words must contain("RE")

              words must have size 3
          }

      }

    }

    "build multiple words from letters placed adjacent to other squares from vertically placed letters " in {
      //   print("mv: " + gibberishWordsMove.formedWords)

      gibberishWordsMove flatMap {
        move =>

          move.formedWords.map {
            built =>
              val words = builtToStr(built)

              words must contain("TTESTS")
              words must contain("TC")
              words must contain("ER")
              words must contain("SE")
              words must contain("TS")

              words must have size 5
          } toOption
      } must beSome

    }

    "extend an existing word horizontally" in {
      val place = toPlace("tares", true, pos(8, 8))

      withGameAndPositions(playedGame, place) {
        move =>
          move.formedWords map {
            built =>
              val words = builtToStr(built)

              words must contain("STARES")
              words must have size 1
          }

      }

    }

    "extend an existing word vertically" in {
      val place = toPlace("sdf", false, pos(7, 9))

      withGameAndPositions(playedGame, place) {
        move =>
          move.formedWords map {
            built =>
              val words = builtToStr(built)

              words must contain("SCORESSDF")
              words must have size 1
          }
      }

    }

    "extend an existing word on the left and right" in {
      val place = addPlaceLists(toPlace("SM", true, pos(1, 5)), toPlace("S", true, pos(10, 5)))
      withGameAndPositions(playedGame, place) {
        move =>
          move.formedWords map {
            built =>
              val words = builtToStr(built)

              words must contain("SMHISTORYS")
              words must have size 1
          }
      }
    }

    "extend an existing word above and below" in {
      val place = addPlaceLists(toPlace("SM", false, pos(7, 1)), toPlace("ST", false, pos(7, 9)))
      withGameAndPositions(playedGame, place) {
        move =>
          move.formedWords map {
            built =>
              val words = builtToStr(built)

              words must contain("SMSCORESST")
              words must have size 1
          }
      }
    }

    "warn about misplaced letters" in {

      val place = toPlace("test", true, pos(1, 1))
      val first = safeUpdateTile(place, 1, Pos.posAt(3, 2), 'C')

      withGameAndPositions(playedGame, place) {
        move =>
          // Square placed outside the 'line' (i.e above)
          Try(move.makeMove must beEqualTo(Failure(MisPlacedLetters(3, 1))))
      }

      val place2 = toPlace("test", true, pos(1, 1))
      val second = safeUpdateTile(place2, 3, pos(5, 1), 'C')

      withGameAndPositions(playedGame, place2) {
        move =>
          // linear, but missing a square to complete the the 'line'
          Try(move.makeMove must beEqualTo(Failure(MisPlacedLetters(5, 1))))
      }

      // Start to complete a word at one side, but misplace letter at the other
      val toPlace3 = addPlaceLists(toPlace("T", true, pos(2, 5)), toPlace("fd", true, pos(11, 5)))
      withGameAndPositions(playedGame, toPlace3) {
        move =>
          Try(move must beEqualTo(Failure(MisPlacedLetters(11, 5))))
      }

    }

    "reject invalid words" in {
      gibberishWordsMove map {
        move =>
          val words = "TTESTS" :: "TC" :: "SE" :: "TS" :: Nil

          move.makeMove.get must throwA[WordsNotInDictionary].like {
            case e: WordsNotInDictionary =>
              e.words must have size 4 // 'RE' should be the only valid word
              e.words must containAllOf(words)
          }
      } must beSome

    }
/*
    "calculate scores correctly" in {

      //  normal
      val placeNormal = toPlace("wa", false, pos(5, 3)) ++ toPlace("p", false, pos(5, 6))
      val normalScore = PlaceLettersMove(playedGame, placeNormal).validate.get.score.get
      normalScore.overAllScore must beEqualTo(9)

      // double letter
      val placeDoubleLetter = toPlace("tyle", true, pos(8, 3))
      val doubleLetterScore = PlaceLettersMove(playedGame, placeDoubleLetter).validate.get.score.get
      doubleLetterScore.overAllScore must beEqualTo(12)

      // Double word
      val doublePlace = toPlace("stair", true, pos(2, 3))
      val mv = PlaceLettersMove(playedGame, doublePlace).validate.get
      val doubleWordScore = mv.score.get
      doubleWordScore.overAllScore must beEqualTo(12)

      // triple letter
      val tripleLetterPlace = toPlace("ale", true, pos(9, 6))
      val tripleLetterScore = PlaceLettersMove(playedGame, tripleLetterPlace).validate.get.score.get
      tripleLetterScore.overAllScore must beEqualTo(10)

      // triple word
      val tripleWordPlace = toPlace("TAO", false, pos(8, 1))
      val tripleWordScore = PlaceLettersMove(playedGame, tripleWordPlace).validate.get.score.get
      tripleWordScore.overAllScore must beEqualTo(11)

      // Multiple words
      val playedFurther = game.copy(board = placeSquares(crossedWords,
        toPlace("wa", false, pos(5, 3)) ++ toPlace("p", false, pos(5, 6))))

      val multipleWordPlace = toPlace("YA", false, pos(6, 2))
      val multipleWordScore = PlaceLettersMove(playedFurther, multipleWordPlace).validate.get.score.get

      multipleWordScore.overAllScore must beEqualTo(19)
      multipleWordScore.individualScores must contain("YA" -> 13)
      multipleWordScore.individualScores must contain("WAS" -> 6)

      // Covering multiple bonus squares
      coversTwoBonuses.score.get.overAllScore must beEqualTo(36)

    }

    "place one letter" in {
      val game1 = PlaceLettersMove(blankGame, toPlace("ravine", true, pos(8, 8))).validate.get.makeMove.get

      val place = (pos(8, 7) -> letterBag.letterFor('O').get :: Nil).toNel.get

      val oneLetterMove = PlaceLettersMove(game1, place).validate.get

      builtToStr(oneLetterMove.formedWords.get) must contain("OR")
      oneLetterMove.formedWords.get must have size 1

      // Horizontal single letter

      // Build multiple words
    }

    "handle blank letters" in {
      // ENVO_NIL

      builtToStr(coversTwoBonuses.formedWords.get) must contain("VENISON")

    }

    val predictableGame = {
      val place = toPlace("lurid", true, pos(8, 8))
      PlaceLettersMove(predictableLetterbagGame, place).validate.get.makeMove.get
    }

    "replace the letters that a player played" in {
      val player = predictableGame.players.get(0).get

      player.letters must containAllOf(toLetters("SV"))
      player.letters must containAllOf(toLetters("EIYUR"))
    }

    "transition the game state correctly" in {
      predictableGame.moves must beEqualTo(1)
      predictableGame.board.LettersRight(pos(7, 8)).map { case (pos, sq, let) => let.letter }.mkString must beEqualTo("LURID")
      predictableGame.bag.lettersAsString must beEqualTo(
        "ADYEICBLEDHMSIXNFERAIWOANETGAELGFIUT_TJHAI_BDONENOECTRIEEREKOAZPVETONSASURAPMNOTO")
      predictableGame.currentPlayer.get.letters.map(_.letter).mkString must beEqualTo("IGQAWLO")
      predictableGame.playersMove must beEqualTo(1)
    }

    "ends the game in the appropriate conditions" in {

    }

    "handle pass moves correctly" in {

    }

    "handle exchange moves correctly" in {

    }
    * 
    */
  }

}
