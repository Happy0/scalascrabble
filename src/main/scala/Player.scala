package scrabble

import scala.util.{ Try, Success, Failure }

case class Player(
  letters: List[Tile],
  username: String,
  score: Int) {

  def replaceLetters(lettrs: List[Tile]): Player = copy(letters = lettrs)

  def exchangeLetters(replace: List[Tile], replaceWith: List[Tile]): Try[Player] =
    if (replace.size == replaceWith.size) accumulateLetters(replace, replaceWith, letters)
    else Failure(MustExchangeSameNumberofLetters())

  def accumulateLetters(replace: List[Tile], replaceWith: List[Tile], playerLetters: List[Tile]): Try[Player] = {
    (replace) match {
      case (x :: xs) =>
        val (before, after) = playerLetters.span(let => let != x)
       
        if (after == Nil) Failure(PlayerDoesNotHaveLettersToExchange()) else {
          accumulateLetters(xs, replaceWith, before ::: after.drop(1))
        }
      case _ => Success(copy(letters = replaceWith ::: playerLetters))

    }
  }

}

