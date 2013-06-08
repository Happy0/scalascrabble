package scrabble

import scala.util.{ Try, Success, Failure }
import scala.util.Failure

import org.specs2.matcher.TraversableMatchers

class MoveTest extends ScrabbleTest {

  val playedGame = game.copy(board = crossedWords, moves = 1)

  val gibberishWordsMove: PlaceLettersMove = {
    val place = toPlace("T", false, pos(6, 4)) ++ toPlace("ESTS", false, pos(6, 6))
    PlaceLettersMove(playedGame, place)
  }

  val blankGame: Game = {
    val str = "JEARVINENVO_NILLEWBKONUIEUWEAZBDESIAPAEOOURGOCDSNIADOAACAR_RMYELTUTYTEREOSITNIRFGPHAQLHESOIITXFDMETG"
    val bag = LetterBag.fromLetters(str, letterBag.tileSet)
    Game.make(List("a", "b", "c", "d"), enDict, bag.get).get
  }

  val coversTwoBonuses: PlaceLettersMove = {
    val game1 = PlaceLettersMove(blankGame, toPlace("ravine", true, pos(8, 8))).makeMove.get
    val place = toPlace("ven", false, pos(11, 5)) ++ toPlace(
      "son", false, pos(11, 9)).updated(0, pos(11, 9) -> BlankLetter('S'))

    PlaceLettersMove(game1, place)
  }

  val modifiedPlayer = playedGame.currentPlayer.replaceLetters(toLetters("tory"))
  val modifiedGame = playedGame.copy(players = playedGame.players.updated(playedGame.playersMove, modifiedPlayer))

  "a move should" should {

    "fail if the first move does not intersect with the star square" in {
      val place = toPlace("test", true, pos(11, 2))

      PlaceLettersMove(game, place).makeMove must beEqualTo(Failure(FirstMovePositionWrong(0)))
    }

    "not place letters on top of occupied squares" in {
      PlaceLettersMove(playedGame, toPlace("hello", true,
        pos(3, 5))).makeMove must beEqualTo(Failure(SquareOccupiedClientError(6)))
    }

    "not place letters that the player does not have" in {
      val place = toPlace("tone", true, pos(8, 3))

      PlaceLettersMove(modifiedGame, place).makeMove must beEqualTo(Failure(playerDoesNotHaveLettersClientError(7)))
    }

    "fail if the word is not attached to an existing word" in {
      val place = toPlace("test", true, pos(1, 1))

      PlaceLettersMove(playedGame, place).makeMove must beEqualTo(Failure(NotAttachedToWord(2)))
    }

    def builtToStr(lists: List[List[(Pos, Tile)]]): List[String] = lists.map { list =>
      list.map { case (pos, letter) => letter.letter }.mkString
    }

    "build multiple words from letters placed adjacent to other squares from horizontally placed letters" in {
      val place = toPlace("o", true, pos(6, 6)) ++ toPlace("e", true, pos(8, 6))
      val mv = PlaceLettersMove(playedGame, place)
      val built = mv.formedWords.get
      val words = builtToStr(built)

      words must contain("TO")
      words must contain("ORE")
      words must contain("RE")

      words must have size 3

    }

    "build multiple words from letters placed adjacent to other squares from vertically placed letters " in {

      val built = gibberishWordsMove.formedWords.get
      val words = builtToStr(built)

      words must contain("TTESTS")
      words must contain("TC")
      words must contain("ER")
      words must contain("SE")
      words must contain("TS")

      words must have size 5

    }

    "extend an existing word horizontally" in {
      val place = toPlace("tares", true, pos(8, 8))
      val mv = PlaceLettersMove(playedGame, place)
      val built = mv.formedWords.get
      val words = builtToStr(built)

      words must contain("STARES")
      words must have size 1
    }

    "extend an existing word vertically" in {
      val place = toPlace("sdf", false, pos(7, 9))
      val mv = PlaceLettersMove(playedGame, place)

      val built = mv.formedWords.get
      val words = builtToStr(built)

      words must contain("SCORESSDF")
      words must have size 1
    }

    "extend an existing word on the left and right" in {
      val place = toPlace("SM", true, pos(1, 5)) ++ toPlace("S", true, pos(10, 5))
      val mv = PlaceLettersMove(playedGame, place)

      val built = mv.formedWords.get
      val words = builtToStr(built)

      words must contain("SMHISTORYS")
      words must have size 1
    }

    "extend an existing word above and below" in {
      val place = toPlace("SM", false, pos(7, 1)) ++ toPlace("ST", false, pos(7, 9))
      val mv = PlaceLettersMove(playedGame, place)

      val built = mv.formedWords.get
      val words = builtToStr(built)

      words must contain("SMSCORESST")
      words must have size 1
    }

    "warn about misplaced letters" in {
      val place = toPlace("test", true, pos(1, 1)).updated(1, pos(3, 2) -> letterFor('C')) // Oh god...
      val mv = PlaceLettersMove(playedGame, place)

      mv.makeMove must beEqualTo(Failure(MisPlacedLetters(3, 1))) // Square placed outside the 'line' (i.e above)

      val mv2 = PlaceLettersMove(playedGame, toPlace("test", true, pos(1, 1)).list.updated(3, pos(5, 1) -> letterFor('C')).toNel.get)
      mv2.makeMove must beEqualTo(Failure(MisPlacedLetters(5, 1))) // linear, but missing a square to complete the the 'line'

      // Start to complete a word at one side, but misplace letter at the other
      val toPlace3 = toPlace("T", true, pos(2, 5)) ++ toPlace("fd", true, pos(11, 5))
      val mv3 = PlaceLettersMove(playedGame, toPlace3)

      mv3.makeMove must beEqualTo(Failure(MisPlacedLetters(11, 5)))
    }

    "reject invalid words" in {
      val res = gibberishWordsMove.makeMove

      val words = "TTESTS" :: "TC" :: "SE" :: "TS" :: Nil

      res.get must throwA[WordsNotInDictionary].like {
        case e: WordsNotInDictionary =>
          e.words must have size 4 // 'RE' should be the only valid word
          e.words must containAllOf(words)
      }
    }

    "calculate scores correctly" in {

      //  normal
      val placeNormal = toPlace("wa", false, pos(5, 3)) ++ toPlace("p", false, pos(5, 6))
      val normalScore = PlaceLettersMove(playedGame, placeNormal).score.get
      normalScore.overAllScore must beEqualTo(9)

      // double letter
      val placeDoubleLetter = toPlace("tyle", true, pos(8, 3))
      val doubleLetterScore = PlaceLettersMove(playedGame, placeDoubleLetter).score.get
      doubleLetterScore.overAllScore must beEqualTo(12)

      // Double word
      val doublePlace = toPlace("stair", true, pos(2, 3))
      val mv = PlaceLettersMove(playedGame, doublePlace)
      val doubleWordScore = mv.score.get
      doubleWordScore.overAllScore must beEqualTo(12)

      // triple letter
      val tripleLetterPlace = toPlace("ale", true, pos(9, 6))
      val tripleLetterScore = PlaceLettersMove(playedGame, tripleLetterPlace).score.get
      tripleLetterScore.overAllScore must beEqualTo(10)

      // triple word
      val tripleWordPlace = toPlace("TAO", false, pos(8, 1))
      val tripleWordScore = PlaceLettersMove(playedGame, tripleWordPlace).score.get
      tripleWordScore.overAllScore must beEqualTo(11)

      // Multiple words
      val playedFurther = game.copy(board = placeSquares(crossedWords,
        toPlace("wa", false, pos(5, 3)) ++ toPlace("p", false, pos(5, 6))))

      val multipleWordPlace = toPlace("YA", false, pos(6, 2))
      val multipleWordScore = PlaceLettersMove(playedFurther, multipleWordPlace).score.get

      multipleWordScore.overAllScore must beEqualTo(19)
      multipleWordScore.individualScores must contain("YA" -> 13)
      multipleWordScore.individualScores must contain("WAS" -> 6)

      // Covering multiple bonus squares
      coversTwoBonuses.score.get.overAllScore must beEqualTo(36)

    }

    "place one letter" in {
      val game1 = PlaceLettersMove(blankGame, toPlace("ravine", true, pos(8, 8))).makeMove.get

      val place = (pos(8, 7) -> letterBag.letterFor('O').get :: Nil).toNel.get

      val oneLetterMove = PlaceLettersMove(game1, place)

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
      PlaceLettersMove(predictableLetterbagGame, place).makeMove.get
    }

    "replace the letters that a player played" in {
      val player = predictableGame.players.get(0).get

      player.letters must containAllOf(toLetters("SV"))
      player.letters must containAllOf(toLetters("EIYUR"))
    }

    "transition the game state correctly" in {
      predictableGame.moves must beEqualTo(1)
      predictableGame.board.LettersRight(pos(7, 8)).map { case (pos, let) => let.letter }.mkString must beEqualTo("LURID")
      predictableGame.bag.lettersAsString must beEqualTo(
        "ADYEICBLEDHMSIXNFERAIWOANETGAELGFIUT_TJHAI_BDONENOECTRIEEREKOAZPVETONSASURAPMNOTO")
      predictableGame.currentPlayer.letters.map(_.letter).mkString must beEqualTo("IGQAWLO")
      predictableGame.playersMove must beEqualTo(1)
    }

    "ends the game in the appropriate conditions" in {

    }

    "handle pass moves correctly" in {

    }

    "handle exchange moves correctly" in {

    }

  }

}