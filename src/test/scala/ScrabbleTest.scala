package scrabble
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import scalaz.NonEmptyList
import scala.util.{ Try, Success, Failure }
import scalaz.Lists
import scalaz.NonEmptyLists

trait ScrabbleTest extends Specification with NonEmptyLists with Lists {
  val enDict = Dictionary.load("Dict/en.txt")

  val board = Board.init

  val letterBag = LetterBag.init

  def pos(x: Int, y: Int) = Pos.posAt(x, y)

  val game = Game.make(List("jim", "joe"), enDict, LetterBag.init)

  def letterFor(c: Char) = letterBag.tileSet.get(c.toUpper)

  def toLetters(str: String): List[Tile] = str.toUpperCase.toList.map(c => letterFor(c)) flatten

  val predictableLetterBag = LetterBag.fromLetters(
    "LISVURDIGQAWLOEIYURADYEICBLEDHMSIXNFERAIWOANETGAELGFIUT_TJHAI_BDONENOECTRIEEREKOAZPVETONSASURAPMNOTO",
    LetterBag.init.tileSet)

  val predictableLetterbagGame = {
    predictableLetterBag flatMap {
      bag =>
        Game.make(List("jim", "joe"), enDict, bag)
    }
  }
  
  def safeUpdateTile(list: Option[NonEmptyList[(Pos, Tile)]], i: Int, tileReplace: Tile) = {
    list flatMap {
      list => list.list.zipWithIndex map {
        case ((pos, tile), index) =>
          if (index == i) pos -> tileReplace else pos -> tile
      } toNel
    }
  }

  def safeUpdateTile(list: Option[NonEmptyList[(Pos, Tile)]], i: Int, posReplace: Option[Pos], char: Char) = {
    list flatMap {
      list =>
        list.list.zipWithIndex flatMap {
          case ((pos, tile), index) =>
            letterFor(char) flatMap {
              c =>
                posReplace map {
                  posReplace =>
                    if (index == i) (posReplace, c) else (pos, tile) // Oh god...

                }

            }
        } toNel
    }

  }

  def addPlaceLists(place1: Option[NonEmptyList[(Pos, Tile)]], place2: Option[NonEmptyList[(Pos, Tile)]]) = {

    place1 |@| place2 apply {
      case (a, b) => a :::> b.list
    }

  }

  // Helper method to place a spread of letters on the board
  def toPlace(word: String, horizontal: Boolean, from: Option[Pos]): Option[NonEmptyList[(Pos, Tile)]] = {

    from flatMap {
      from =>
        val positions = (if (!horizontal) (from.y to from.y + word.size).map(c => pos(from.x, c))
        else (from.x to from.x + word.size).map(c => pos(c, from.y))).flatten

        (positions zip toLetters(word)).toList.toNel
    }

  }

  val crossedWords: Option[Board] = {
    val horPlacements = toPlace("history", true, pos(3, 5))
    val downPlacements = toPlace("scores", false, pos(7, 3))

    placeSquares(board, horPlacements) flatMap {
      boa => placeSquares(boa, downPlacements)
    }

  }

  /** Place tiles on the board at the specified positions */
  def placeSquares(board: Board, placed: Option[NonEmptyList[(Pos, Tile)]]): Option[Board] =
    {
      placed flatMap {
        placed =>
          placed.list.foldLeft[Option[Board]](Some(board)) {
            case (Some(b), placed) =>
              b.placeLetter(placed._1, placed._2)
            case (None, _) => None
          }
      }
    }

  implicit def pimpNonEmptyList[A](nel: NonEmptyList[A]) = G(nel)

  case class G[A](nel: NonEmptyList[A]) {
    def ++(b: NonEmptyList[A]): NonEmptyList[A] = nel :::> b.list

    def updated(index: Int, elem: A): NonEmptyList[A] = nel.list.updated(index, elem).toNel.get
  }

  /** A game which is in progress - or is 'constructed' to a certain scenario for the purposes of testing */
  implicit def playedGame(game: Game) = new {

    /** Place words on the board*/
    def playMoves(placed: List[(Pos, Tile)]*): Try[Game] = ???

  }

}