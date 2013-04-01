package scrabble

//@TODO: Think about how to generalise this to other languages. Perhaps using configuration files...

/** tiles: The current tiles in the bag */
case class LetterBag(letters: List[Letter], size: Int) {

  override def toString = letters.toString

  /**
   * Remove @num letters from the letter bag. Returns a list of removed letters (if available)
   *  and the resulting letter bag
   */
  def remove(num: Int): (List[Letter], LetterBag) = {
    val split = letters.splitAt(num - 1)
    val removedLetters = split._1
    val newSize = if (size - num <= 0) 0 else size - num;
    val newBag = LetterBag(split._2, newSize)

    (removedLetters, newBag)
  }
  
  /** Exchange @exchanged letters for the same number of letters from the bag. Returns the new bag after shuffling its contents. */
  def exchange(exchanged: List[Letter]) : (List[Letter],LetterBag) =
    {
	  val (given, bag) = remove(exchanged.size)
	  (given, LetterBag(util.Random.shuffle(bag.letters ::: exchanged), size))
    }
    

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
    val tenPoints = List('Q', 'Z').map(ch => (ch, 10, 10))

    val all: List[(Char, Int, Int)] = blankPoints ::: onePoints ::: twoPoints ::: threePoints ::: fourPoints ::: fivePoints ::: eightPoints ::: tenPoints

    // Yield a list of all the letters in the bag, using the distribution to yield the right number of letters
    val letters = all.foldLeft(List.empty[Letter]) {
      case (list, (chr: Char, vl: Int, dst: Int)) =>
        list ::: List.fill(dst)(Letter(chr, vl))
    }

    // Construct with a randomised list
    LetterBag(util.Random.shuffle(letters), letters.size)
  }
  
  //@TODO: Placeholder for other language generalisation
  def apply(filePath: String): LetterBag = init

  def main(args: Array[String]) {
    val bag = LetterBag.init
    println(bag)
  }
}