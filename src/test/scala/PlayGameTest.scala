package scrabble;
import scalaz.NonEmptyList

import scala.util.{ Try, Success, Failure }

/**
 *  @letterString: What the letterbag should be after the move
 *  @place: Placed for the move
 *  @newRack: What the player's rack should be after the move
 *  @score: What the player's score should be after the move
 */
case class Step(letterString: String, place: Option[NonEmptyList[PosTile]], newRack: List[Tile], score: Int)

class PlayGameTest extends ScrabbleTest {

  val steps: List[Option[NonEmptyList[PosTile]]] = List(
    toPlace("RAVINE", true, pos(8, 8)),
    safeUpdateTile(toPlace("OVEL", false, pos(12, 9)), 3, BlankLetter('L')),
    addPlaceLists(toPlace("W", true, pos(9, 7)), toPlace("KE", false, pos(9, 9))),
    toPlace("N", true, pos(11, 9)),
    addPlaceLists(toPlace("B", true, pos(13, 7)), toPlace("D", true, pos(13, 9))),
    toPlace("NAI", true, pos(9, 12)),
    addPlaceLists(toPlace("B", true, pos(11, 11)), toPlace("LLE", true, pos(13, 11))),
    toPlace("WEE", false, pos(10, 13)),
    addPlaceLists(toPlace("JA", false, pos(15, 9)), toPlace("GERS", false, pos(15, 12))),
    addPlaceLists(toPlace("CANOPI", true, pos(4, 15)), toPlace("D", true, pos(11, 15))),
    toPlace("SONI", false, pos(4, 11)),
    toPlace("AUDIO", false, pos(3, 10)),
    safeUpdateTile(toPlace("RAZER", false, pos(5, 8)), 3, BlankLetter('E')),
    toPlace("MULEY", false, pos(2, 6)),
    toPlace("ROOTY", false, pos(3, 2)),
    toPlace("ETUIS", false, pos(14, 4)),
    toPlace("RACING", false, pos(1, 10)),
    toPlace("HATP", false, pos(11, 4)),
    toPlace("HAES", false, pos(12, 2)),
    toPlace("DOUX", false, pos(15, 1)),
    toPlace("GEM", false, pos(13, 1)),
    addPlaceLists(toPlace("Q", true, pos(4, 9)), toPlace("T", true, pos(6, 9))),
    toPlace("IO", false, pos(6, 13)),
    toPlace("FIT", false, pos(10, 2)))

  val tryGame = blankGame.toTry(Failure(UnlikelyInternalError())) { gm => Success(gm) }

  val makeSteps = steps.foldLeft(tryGame) {
    case (game, place) =>

      game flatMap {
        gm =>
          place.toTry(Failure(UnlikelyInternalError())) {
            place =>
              PlaceLettersMove(gm, place).validate flatMap (_.makeMove)
          }
      }
  }

  "a playtest" should {

    "keep a log properly" in {
      makeSteps match {
        case Success(game) =>
          game.log must beSome
          game.log foreach { log => log.moveHistory.list.size must beEqualTo(steps.size) }

        case Failure(x) =>
      }
    }

    "successfully complete a full game with the right state transitions" in {

      makeSteps match {
        case Success(game) =>
          game.bag.size must beEqualTo(0)
          game.gameEnded must beEqualTo(true)
          game.moves must beEqualTo(24)

          val player1 = game getPlayer (0)
          val player2 = game getPlayer (1)
          val player3 = game getPlayer (2)
          val player4 = game getPlayer (3)

          player1 must not beNone

          player2 must not beNone

          player3 must not beNone

          player4 must not beNone

          player1.foreach {
            player =>
              player.score must beEqualTo(189)
              player.letters.map(t => t.letter).mkString must be equalTo ("TF")
          }

          player2.foreach {
            player =>
              player.score must beEqualTo(186)
              player.letters.map(t => t.letter).mkString must beEqualTo("L")
          }

          player3.foreach {
            player =>
              player.score must beEqualTo(110)
              player.letters.map(t => t.letter).mkString must beEqualTo("E")
          }

          player4.foreach {
            player =>
              player.score must beEqualTo(57 + 77 + 20)
              player.letters.map(t => t.letter).mkString must beEqualTo("")

          }

          println(game.board)
          game.status must beEqualTo(Ended)

        // well, uh.. this is a hardly ideal hack. there must be a way to extend specs2 appropriately.
        case x => x must beEqualTo(9902131)

      }
    }

  }

}