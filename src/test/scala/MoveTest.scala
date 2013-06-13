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
            // safeUpdateTile(list, i, pos, char)

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

    def checkMisplaced(game: Option[Game], place: Option[NonEmptyList[PosTile]], errorAt: (Int, Int)) = {
      game flatMap {
        game =>
          place flatMap {
            place =>
              PlaceLettersMove(game, place).validate map {
                move =>
                  move.formedWords must beEqualTo(Failure(MisPlacedLetters(errorAt._1, errorAt._2)))
              } toOption
          }
      } must beSome
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

    "calculate scores correctly" in {

      //  normal
      val placeNormal = addPlaceLists(toPlace("wa", false, pos(5, 3)), toPlace("p", false, pos(5, 6)))
      withGameAndPositions(playedGame, placeNormal)(_.score map (_.overAllScore must beEqualTo(9)))

      // double letter
      val placeDoubleLetter = toPlace("tyle", true, pos(8, 3))
      withGameAndPositions(playedGame, placeDoubleLetter)(_.score map (_.overAllScore must beEqualTo(12)))

      // Double word
      val doublePlace = toPlace("stair", true, pos(2, 3))
      withGameAndPositions(playedGame, doublePlace)(_.score map (_.overAllScore must beEqualTo(12)))

      // triple letter
      val tripleLetterPlace = toPlace("ale", true, pos(9, 6))
      withGameAndPositions(playedGame, tripleLetterPlace)(_.score map (_.overAllScore must beEqualTo(10)))

      // triple word
      val tripleWordPlace = toPlace("TAO", false, pos(8, 1))
      withGameAndPositions(playedGame, tripleWordPlace)(_.score map (_.overAllScore must beEqualTo(11)))

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

      val multipleWordPlace = toPlace("YA", false, pos(6, 2))
      withGameAndPositions(playedFurther, multipleWordPlace) {
        _.score map {
          score =>
            score.overAllScore must beEqualTo(19)
            score.individualScores must contain("YA" -> 13)
            score.individualScores must contain("WAS" -> 6)
        }
      }

      // Covering multiple bonus squares
      coversTwoBonuses map (_.score map (_.overAllScore must beEqualTo(36))) must beSome

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

    "place one letter" in {

      val game1 = furtherGame(blankGame, toPlace("ravine", true, pos(8, 8)))

      val place = toPlace("O", true, pos(8, 7))

      //val oneLetterMove = PlaceLettersMove(game1, place).validate.get

      withGameAndPositions(game, place) {
        move =>
          move.formedWords map {
            built =>
              val words = builtToStr(built)
              words must contain("OR")
              words must have size 1
          }
      }

      // @TODO: Horizontal single letter

      // @TODO: Build multiple words
    }

    "handle blank letters" in {
      // ENVO_NIL

      coversTwoBonuses flatMap {
        _.formedWords map {
          words =>
            builtToStr(words) must contain("VENISON")
        } toOption
      } must beSome

    }

    val predictableGame = {
      val place = toPlace("lurid", true, pos(8, 8))
      furtherGame(predictableLetterbagGame, place)
    }

    "replace the letters that a player played" in {
      predictableGame flatMap {
        predictableGame =>
          val player = predictableGame.players.get(0)

          player map {
            player =>
              player.letters must containAllOf(toLetters("SV"))
              player.letters must containAllOf(toLetters("EIYUR"))
          }

      } must beSome

    }

    "transition the game state correctly" in {
      predictableGame map {
        predictableGame =>
          predictableGame.moves must beEqualTo(1)
          pos(7, 8) map {
            pos =>
              predictableGame.board.LettersRight(pos).map { case (pos, sq, let) => let.letter }.mkString must beEqualTo("LURID")
          } must beSome

          predictableGame.bag.lettersAsString must beEqualTo(
            "ADYEICBLEDHMSIXNFERAIWOANETGAELGFIUT_TJHAI_BDONENOECTRIEEREKOAZPVETONSASURAPMNOTO")
          predictableGame.currentPlayer map { _.letters.map(_.letter).mkString must beEqualTo("IGQAWLO") } must beSome
          predictableGame.playersMove must beEqualTo(1)
      } must beSome

    }

    "ends the game in the appropriate conditions" in {

    }

    "handle pass moves correctly" in {

    }

    "handle exchange moves correctly" in {

    }
  }

}
