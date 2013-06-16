package scrabble

import scala.util.{ Try, Success, Failure }

case class Player(
  letters: List[Tile],
  username: String,
  score: Int) {

  def replaceLetters(lettrs: List[Tile]): Player = copy(letters = lettrs)

  def hasLetters(tiles: List[Tile]) = (letters intersect tiles).size == tiles.size

  /** Removes letters from player's rack. Returns None if the player does not have all the input tiles */
  def removeLetters(tiles: List[Tile]) = {

    if (!hasLetters(tiles)) None else Some(copy(letters = letters diff tiles))
  }

  def exchangeLetters(replace: List[Tile], replaceWith: List[Tile]): Try[Player] =
    if (replace.size == replaceWith.size) {

      removeLetters(replace).toTry(Failure(PlayerDoesNotHaveLettersToExchange())) {
        playr => Success(playr.copy(letters = replaceWith ::: playr.letters))
      }

    } else Failure(MustExchangeSameNumberofLetters())

}

