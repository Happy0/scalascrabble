package scrabble

import scala.util.{ Try, Success, Failure }

case class Player(
  letters: List[Tile],
  username: String,
  score: Int) {

  def replaceLetters(lettrs: List[Tile]): Player = copy(letters = lettrs)

  def exchangeLetters(replace: List[Tile], replaceWith: List[Tile]): Try[Player] =
    if (replace.size == replaceWith.size) accumulateLetters(replace, replaceWith, Nil) else Failure(MustExchangeSameNumberofLetters())

  def accumulateLetters(replace: List[Tile], replaceWith: List[Tile], newRack: List[Tile]): Try[Player] = {
    (replace, replaceWith) match {
      case (x :: xs, y :: ys) =>
        val (before, after) = letters.span(let => let != x)
        if (after == Nil) Failure(PlayerDoesNotHaveLettersToExchange()) else {
          accumulateLetters(xs, ys, before ::: (y :: after.drop(1)))
        }
      case _ => Success(copy(letters = newRack))

    }
  }

}

