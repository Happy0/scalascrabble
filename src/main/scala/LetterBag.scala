package scrabble

//@TODO: Think about how to generalise this to other languages. Perhaps using configuration files...

/** tiles: The current tiles in the bag */
case class LetterBag(letters: List[Tile], size: Int, tileSet: Map[Char, Letter]) {

  override def toString = letters.toString

  /**
   * Remove @num letters from the letter bag. Returns a list of removed letters (if available)
   *  and the resulting letter bag
   */
  def remove(num: Int): (List[Tile], LetterBag) = {
    val split = letters.splitAt(num)
    val removedLetters = split._1
    val newSize = if (size - num <= 0) 0 else size - num;
    val newBag = copy(letters = split._2, size = newSize)

    (removedLetters, newBag)
  }

  /**
   * Exchange @exchanged letters for the same number of letters from the bag. Returns None if there is
   *   not enough letters in the bag to exchange. Otherwise, returns the new bag after shuffling its contents.
   */
  def exchange(exchanged: List[Tile]): Option[(List[Tile], LetterBag)] = {
    if (exchanged.size > size) None else {

      val (given, bag) = remove(exchanged.size)
      val newLetters = util.Random.shuffle(exchanged ::: bag.letters)
      Some((given, copy(letters = newLetters)))
    }

  }

  def letterFor(letter: Char): Option[Letter] = tileSet get letter

}

object LetterBag {

  /** Returns a new LetterBag in its intial state. List is in randomised order. */
  val init: LetterBag = {

    // (Letter, Value, Distribution)
    val blankPoints = List(('_', 0, 2))
    val onePoints = List('E', 'A', 'I', 'O', 'N', 'R', 'T', 'L', 'S', 'U') zip List(12, 9, 9, 8, 6, 6, 6, 4, 4, 4) map { case (x, y) => (x, 1, y) }
    val twoPoints = List(('D', 2, 4), ('G', 2, 3))
    val threePoints = List('B', 'C', 'M', 'P').map(ch => (ch, 3, 2))
    val fourPoints = List('F', 'H', 'V', 'W', 'Y').map(ch => (ch, 4, 2))
    val fivePoints = List('K').map(ch => (ch, 5, 1))
    val eightPoints = List('J', 'X').map(ch => (ch, 8, 1))
    val tenPoints = List('Q', 'Z').map(ch => (ch, 10, 1))

    val all: List[(Char, Int, Int)] = blankPoints ::: onePoints ::: twoPoints ::: threePoints ::: fourPoints ::: fivePoints ::: eightPoints ::: tenPoints

    // Yield a list of all the letters in the bag, using the distribution to yield the right number of letters
    val letters = all.foldLeft(List.empty[Tile]) {
      case (list, (chr: Char, vl: Int, dst: Int)) =>
        list ::: List.fill(dst)(if (chr == '_') BlankLetter(chr) else Letter(chr, vl))
    }

    val tileSet: Map[Char, Letter] = all.map { case (chr, vl, dst) => chr -> Letter(chr, vl) } toMap

    // Construct with a randomised list
    LetterBag(util.Random.shuffle(letters), letters.size, tileSet)
  }

  //@TODO: Placeholder for other language generalisation
  def apply(filePath: String): LetterBag = ???

  def main(args: Array[String]) {
    val bag = LetterBag.init
    println(bag)
  }
}