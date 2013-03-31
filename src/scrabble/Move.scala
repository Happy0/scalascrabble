package scrabble

case class Move(game: Game, placed: List[(Pos, Letter)], blanks: List[Char]) {

  /** Returns the updated game if the move is a valid scrabble move, otherwise returns a String with an explanation of why the move is invalid */
  //def updatedGame: Either[String, Game] = {}

  private val playerHasLetters: Boolean =
    {
      val doesntHave = placed.find { case (Pos(x, y, gr), let) => !game.currentPlayer.letters.contains(let) }
      doesntHave match {
        case None => true
        case Some(x) => false
      }
      
      
    }
  
  //private val hasRepeats
  
  // Returns true if the letter are placed in a legal distribution (linear or horizontal) within the board range, and there are no already occupied squares
  def validSpread: Boolean = {
    val horizontal = placed(0)._1.x == placed(placed.size - 1)._1.x
    val vertical = placed(0)._1.y == placed(placed.size - 1)._1.y

    def isValid: (Boolean, Int, Int) = {
      placed.foldLeft(true, placed(0)._1.x, placed(0)._1.y) {
        case ((bool: Boolean, lastx: Int, lasty: Int), (Pos(x, y, ch), let)) =>

          val curPos = Pos.posAt(x, y)
          if (!curPos.isDefined) return (false, x, y) // If the position given isn't in range of the board, return false

          if (!game.board.squareAt(curPos.get).isEmpty) return (false, x, y) // If the square is already occupied, return false

          if (horizontal) {
            val hor = y == placed(0)._1.y

            lazy val pos = Pos.posAt(lastx + 1, y)
            lazy val defined = pos.isDefined
            lazy val occupied = defined && !game.board.squareAt(pos.get).isEmpty

            val correcty = (y == y + 1 || occupied)
            if (hor && correcty) (true, x, y) else return { (false, x, y) }

          } else if (vertical) {
            val ver = y == placed(0)._1.y
            lazy val pos = Pos.posAt(lastx + 1, y)
            lazy val defined = pos isDefined
            lazy val occupied = defined && !game.board.squareAt(pos.get).isEmpty
            val correctx = (x == x + 1 || occupied)

            if (ver && correctx) (true, x, y) else return { (false, x, y) }
          } else {
            return { (false, x, y) }
          }
      }
    }

    return isValid._1
  }

  /*
  private val updatedPlayer: Player = 
  
  private val updatedBag = 
    
  private val updatedBoard = 
   
  */

}

object Main {
  def main(args: Array[String]) {
    val game = Game.init(List("jim", "joe"), Dictionary.load("C:\\workspace\\Scala\\scalascrabble\\src\\Dict\\en.txt"), LetterBag.init)

    val placed = List(Pos.posAt(1, 1).get -> Letter('a', '1'),
      Pos.posAt(1, 2).get -> Letter('a', '1'),
      Pos.posAt(1, 3).get -> Letter('a', '1'))
      
    val blanks = List()
    
    val move = Move(game, placed,blanks)
    
    println(move.validSpread)
  }
}


