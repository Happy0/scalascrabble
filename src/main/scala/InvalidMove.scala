package scrabble

abstract class InvalidMove(errorCode: Int) extends Throwable {

  def defaultMessage: String
}

case class FirstMovePositionWrong(errorCode: Int = 0) extends InvalidMove(errorCode) {
  def defaultMessage = "First letter must be placed on the star"
}

case class MisPlacedLetters(x: Int, y: Int, errorCode: Int = 1) extends InvalidMove(errorCode) {
  override def toString = "MisPlacedLetters" + (x, y)

  def defaultMessage = ("Letter placed at " + Pos.at(x, y).get.toString + " starts an illegal move. Must be a linear placement, and attached to a word. ")
}

case class NotAttachedToWord(errorcode: Int = 2) extends InvalidMove(errorcode) {
  def defaultMessage = "Not attached to existing word."
}

case class NotInDictionary(word: String, errorcode: Int = 3) extends InvalidMove(errorcode) {
  def defaultMessage = word + " is not in the dictionary."
}

/** $score : The prospective score, if it was not an invalid word */
case class WordsNotInDictionary(words: List[String], score: Score, errorCode: Int = 4) extends InvalidMove(errorCode) {
  def defaultMessage = words.fold("") { case (str, wrd) => str + " " + wrd } + " are not in the dictionary. "
}

case class NotLinear(errorcode: Int = 5) extends InvalidMove(errorcode) {
  def defaultMessage = "Letters are not placed in a line"
}

case class SquareOccupiedClientError(errorCode: Int = 6) extends InvalidMove(errorCode) {
  def defaultMessage = "Square already occupied. Client error or malicious client."
}

case class playerDoesNotHaveLettersClientError(errorcode: Int = 7) extends InvalidMove(errorcode) {
  def defaultMessage = "Player does not have all of the placed letters. Client error or malicious client."
}

case class LetterNotInTileSetClientError(errorcode: Int = 8) extends InvalidMove(errorcode) {
  def defaultMessage = "Letter not in the tileset for this game. Client error or malicious client."
}

case class MustExchangeSameNumberofLetters(errorcode: Int = 10) extends InvalidMove(errorcode) {
  def defaultMessage = "Not enough letters given to exchange"
}

case class BagNotFullEnoughToExchange(errorcode: Int = 11) extends InvalidMove(errorcode) {
  def defaultMessage = "The bag does not contain enough letters to make this exchange."
}

case class PlayerDoesNotHaveLettersToExchange(errorcode: Int = 12) extends InvalidMove(errorcode) {
  def defaultMessage = "The player does not have these letters to exchange"
}

case class GameHasEnded(errorcode: Int = 13) extends InvalidMove(errorcode) {
  def defaultMessage = "Game cannot be played further as it has ended."
}

case class GameHasNotStarted(errorcode: Int = 14) extends InvalidMove(errorcode) {
  def defaultMessage = "Game has not started"
}

case class UnlikelyInternalError(errorcode: Int = 9) extends InvalidMove(errorcode) {
  def defaultMessage = "The compiler and I had a disagreement, and the compiler won the argument."
}