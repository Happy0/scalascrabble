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
    toPlace("NAI", true, pos(9, 12)))

  "a playtest" should {

    "successfully complete a full game with the right state transitions" in {

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

      makeSteps match {
        case Success(game) =>
          game.players.get(1).foreach { player => println(player.letters.map(_.letter).mkString) }
          println(game.bag.lettersAsString)
          game.getPlayer(1).map(_.score) must beEqualTo(Some(17))

        // well, uh.. this is a hardly ideal hack. there must be a way to extend specs2 appropriately.
        case x => x must beEqualTo(9902131)

      }
    }

  }

}