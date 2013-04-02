package scrabble

abstract class InvalidMove(errorCode: Int) {

  def defaultMessage: String
}

case class FirstMovePositionWrong(errorCode: Int = 0) extends InvalidMove(errorCode) {
  def defaultMessage = "First letter must be placed on the star"
}

case class MisPlacedLetters(x: Int, y: Int, errorCode: Int = 1) extends InvalidMove(errorCode) {

  def defaultMessage = ("Letter placed at " + Pos.posAt(x, y).get.toString + " start an illegal move. Must be a linear placement, and attached to a word. ")
}

case class NotAttachedToWord(errorcode: Int = 2) extends InvalidMove(errorcode) {
  def defaultMessage = "Not attached to existing word."
}

case class NotInDictionary(word: String, errorcode: Int = 3) extends InvalidMove(errorcode) {
  def defaultMessage = word + " is not in the dictionary."
}

case class NotLinear(errorcode: Int = 4) extends InvalidMove(errorcode) {
  def defaultMessage = "Letters are not placed in a line"
}