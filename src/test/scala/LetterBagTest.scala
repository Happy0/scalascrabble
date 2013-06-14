package scrabble

import scalaz.MA
import scala.util.{ Try, Success, Failure }


class LetterBagTest extends ScrabbleTest {

  val lettersOnly = letterBag.letters.map(c => c.letter)

  def getTile(ch: Char, letterBag: LetterBag) = letterBag.tileSet.get(ch)

  def letterAppears(ch: Char, bag: LetterBag): Int = {
    bag.letters.foldLeft(0)((acc, let) =>
      if (let.letter == ch) acc + 1 else acc)
  }

  def bagCorrectlyDistributed(letterBag: LetterBag) = {
    letterAppears('A', letterBag) must beEqualTo(9)
    letterAppears('B', letterBag) must beEqualTo(2)
    letterAppears('C', letterBag) must beEqualTo(2)
    letterAppears('D', letterBag) must beEqualTo(4)
    letterAppears('E', letterBag) must beEqualTo(12)
    letterAppears('F', letterBag) must beEqualTo(2)
    letterAppears('G', letterBag) must beEqualTo(3)
    letterAppears('H', letterBag) must beEqualTo(2)
    letterAppears('I', letterBag) must beEqualTo(9)
    letterAppears('J', letterBag) must beEqualTo(1)
    letterAppears('K', letterBag) must beEqualTo(1)
    letterAppears('L', letterBag) must beEqualTo(4)
    letterAppears('M', letterBag) must beEqualTo(2)
    letterAppears('N', letterBag) must beEqualTo(6)
    letterAppears('O', letterBag) must beEqualTo(8)
    letterAppears('P', letterBag) must beEqualTo(2)
    letterAppears('Q', letterBag) must beEqualTo(1)
    letterAppears('R', letterBag) must beEqualTo(6)
    letterAppears('S', letterBag) must beEqualTo(4)
    letterAppears('T', letterBag) must beEqualTo(6)
    letterAppears('U', letterBag) must beEqualTo(4)
    letterAppears('V', letterBag) must beEqualTo(2)
    letterAppears('W', letterBag) must beEqualTo(2)
    letterAppears('X', letterBag) must beEqualTo(1)
    letterAppears('Y', letterBag) must beEqualTo(2)
    letterAppears('Z', letterBag) must beEqualTo(1)
  }

  def bagCorrectlyValued(letterBag: LetterBag) = {
    getTile('A', letterBag) map (_.value) must beSome(1)
    getTile('B', letterBag) map (_.value) must beSome(3)
    getTile('C', letterBag) map (_.value) must beSome(3)
    getTile('D', letterBag) map (_.value) must beSome(2)
    getTile('E', letterBag) map (_.value) must beSome(1)
    getTile('F', letterBag) map (_.value) must beSome(4)
    getTile('G', letterBag) map (_.value) must beSome(2)
    getTile('H', letterBag) map (_.value) must beSome(4)
    getTile('I', letterBag) map (_.value) must beSome(1)
    getTile('J', letterBag) map (_.value) must beSome(8)
    getTile('K', letterBag) map (_.value) must beSome(5)
    getTile('L', letterBag) map (_.value) must beSome(1)
    getTile('M', letterBag) map (_.value) must beSome(3)
    getTile('N', letterBag) map (_.value) must beSome(1)
    getTile('O', letterBag) map (_.value) must beSome(1)
    getTile('P', letterBag) map (_.value) must beSome(3)
    getTile('Q', letterBag) map (_.value) must beSome(10)
    getTile('R', letterBag) map (_.value) must beSome(1)
    getTile('S', letterBag) map (_.value) must beSome(1)
    getTile('T', letterBag) map (_.value) must beSome(1)
    getTile('U', letterBag) map (_.value) must beSome(1)
    getTile('V', letterBag) map (_.value) must beSome(4)
    getTile('W', letterBag) map (_.value) must beSome(4)
    getTile('X', letterBag) map (_.value) must beSome(8)
    getTile('Y', letterBag) map (_.value) must beSome(4)
    getTile('Z', letterBag) map (_.value) must beSome(10)
  }

  "a letterbag should" should {

    "contain the right number of each letter" in {
      bagCorrectlyDistributed(letterBag)
    }

    "have the right score for each Tile" in {
      bagCorrectlyValued(letterBag)
    }

    "contain 100 letter tiles" in {
      letterBag.letters must have size 100
    }

    def transitionBagProperly(removed: List[Tile], newBag: LetterBag, oldBag: LetterBag): Boolean = {
      removed.foldLeft(true) { (bool, let) =>
        if (bool == false) return false
        else {
          val removedTimes = removed.filter(_ == let) size

          letterAppears(let.letter, newBag) == letterAppears(let.letter, oldBag) - removedTimes
        }

      }
    }

    "properly remove letters from the bag" in {
      val (removed, newBag) = letterBag.remove(3)

      newBag.letters must have size 97
      removed must have size 3

      transitionBagProperly(removed, newBag, letterBag)

      val nextBag = newBag.remove(5)
      nextBag._2.letters must have size 92

      transitionBagProperly(nextBag._1, nextBag._2, newBag)
    }

    "cope with boundary removal conditions" in {
      val (_, bag) = letterBag.remove(95)
      val (removed, boundaryBag) = bag.remove(7)

      removed.size must beEqualTo(5)
      boundaryBag.letters must beEmpty

    }

    def exchangesProperly(exchanged: List[Tile], originalBag: LetterBag): Unit = {
      val exch = letterBag.exchange(exchanged) map {
        case (received, newbag) =>

          exchanged.foreach {
            c =>
              val newAmount = letterAppears(c.letter, newbag)

              newAmount must beEqualTo((letterAppears(c.letter, letterBag) + exchanged.filter(_ == c).size)
                - received.filter(_ == c).size)

          }
          newbag.size must beEqualTo(letterBag.size)
      }

      exch must not be equalTo(Failure(BagNotFullEnoughToExchange()))
    }

    def strToLetters(str: String) = str.toList.map { c => letterBag.tileSet.get(c) }.flatten

    "properly exchange letters" in {
      val exchangedWithSingle = strToLetters("ABCDE")
      exchangesProperly(exchangedWithSingle, letterBag)

      val exchangedWithDoubleLetters = strToLetters("AABCDD")
      exchangesProperly(exchangedWithDoubleLetters, letterBag)

    }

    "fail to exchange when there is not enough letters in the bag to exchange for" in {
      val (_, smallBag) = letterBag.remove(98)

      smallBag.exchange(strToLetters("AABCDD")) must beEqualTo(Failure(BagNotFullEnoughToExchange()))

    }

    "keep track of the letter bag's size correctly" in {
      val (_, newBag) = letterBag.remove(6)
      newBag.size must beEqualTo(94)

      val (_, emptyBag) = newBag.remove(100)
      emptyBag.size must beEqualTo(0)

      val exchangedWithSingle = strToLetters("ABCDE")
      val res = newBag.exchange(exchangedWithSingle) map {
        case (_, exchBag) =>
          exchBag.size must beEqualTo(94)
      }

      res must not be equalTo(Failure(BagNotFullEnoughToExchange()))

    }

    "construct a bag from a string of letters" in {
      predictableLetterBag map {
        bag =>
          bagCorrectlyValued(bag)
          bagCorrectlyDistributed(bag)
      } must beSome

    }

  }

}