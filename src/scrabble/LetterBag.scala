package scrabble

/** tiles: The current tiles in the bag */
class LetterBag(tiles: List[Letter]) {

}

object LetterBag {

  def apply(tiles: List[Letter]): LetterBag = LetterBag(tiles)

  /** Returns a new LetterBag in its intial state */
  val init: LetterBag = {
    val blankPoints = List(('_', 0, 2))
    val onePoints = List('E', 'A', 'I', 'O', 'N', 'R', 'T', 'L', 'S', 'U') zip List(12, 9, 9, 8, 6, 6, 6, 4, 4, 4) map { case (x, y) => (x, 1, y) }
    val twoPoints = List(('D', 2, 4), ('G', 2, 3))
    val threePoints = List('B', 'C', 'M', 'P').map(ch => (ch, 3, 2))
    val fourPoints = List('F', 'H', 'V', 'W', 'Y').map(ch => (ch, 4, 2))
    val fivePoints = List('K').map(ch => (ch, 5, 1))
    val eightPoints = List('J', 'X').map(ch => (ch, 8, 1))
    val tenPoints = List('Q', 'Z').map(ch => (ch, 10, 10))

    val all = blankPoints ::: onePoints ::: twoPoints ::: threePoints ::: fourPoints ::: fivePoints ::: eightPoints ::: tenPoints

    // Yield a list of letters, using the distribution to yield the right number of letters
    val letters = all.foldLeft(List.empty[Letter]){
      case (list, (let,vl, dst)) => 
        list ++ List.tabulate(dst)(i => Letter(let, vl))
    }
    
    LetterBag(util.Random.shuffle(letters))
  }

}