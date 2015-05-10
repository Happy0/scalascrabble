package scrabble

import scala.util.{ Try, Success, Failure }

case class FormedWords(mainWord: Word, adjacentWords: List[Word]) {

  def prependToMainWord(posSquare: PosSquare): FormedWords = copy(mainWord = posSquare :: mainWord)

  def prependToMainWord(word: Word): FormedWords = copy(mainWord = word ::: mainWord)

  def addAdjacentWord(word: Word): FormedWords = copy(adjacentWords = word :: adjacentWords)

  lazy val getWords = mainWord :: adjacentWords

  def calculateScore(dictionary: Dictionary, sevenLetterBonus: Boolean): Try[Score] = {
    def toWord(list: List[(Pos, Square, Tile)]): String = list.map { case (pos, sq, tile) => tile.letter }.mkString

    val (score, lsts, badwords) = getWords.foldLeft((0, List.empty[(String, Int)], List.empty[String])) {
      case ((acc, lsts, badwords), xs) =>
        val word = toWord(xs)
        val bdwords = if (!dictionary.isValidWord(word)) word :: badwords else badwords
        // Sort to put the word bonus squares last
        val (score, wordBonuses) = xs.foldLeft(0, List.empty[Int => Int]) {
          case ((scr, wordBonuses), (pos, sq, tile)) =>

            // If the bonus has already been used, ignore the bonus square
            sq.tile.fold {
              sq match {
                case NormalSquare(_) => (scr + tile.value, wordBonuses)
                case DoubleLetterSquare(_) => (scr + (tile.value * 2), wordBonuses)
                case TripleLetterSquare(_) => (scr + (tile.value * 3), wordBonuses)
                case DoubleWordSquare(_) => (scr + tile.value, ((i: Int) => i * 2) :: wordBonuses)
                case TripleWordSquare(_) => (scr + tile.value, ((i: Int) => i * 3) :: wordBonuses)
              }
            }(currentTile =>
              (scr + currentTile.value, wordBonuses))
        }

        val finalScore = wordBonuses.foldLeft(score) { case (score, func) => func(score) }

        (acc + finalScore, (word, finalScore) :: lsts, bdwords)

    }
    val theScore: Score = if (sevenLetterBonus) Score(score + 50, lsts) else Score(score, lsts)

    if (badwords.isEmpty) Success(theScore) else Failure(WordsNotInDictionary(badwords, theScore))

  }

}