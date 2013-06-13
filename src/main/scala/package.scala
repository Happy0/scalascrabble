import scalaz.NonEmptyList

package object scrabble extends scalaz.Zeros with scalaz.MABs {

  type PosTile = (Pos,Tile)
  type PosSquare = (Pos, Square, Tile)
  type PosSquares = List[PosSquare]
  type Direction = Pos => Option[Pos]

  implicit final class ScrabblePimpedOption[A](o: Option[A]) {

    def ??[B: scalaz.Zero](f: A => B): B = o.fold(mzero[B])(f)
    
    
  }
  
  implicit final class ScrabblePimpedNonEmptyList[A](nel : NonEmptyList[A]) {
    
    def last : A = nel.tail.lastOption getOrElse nel.head
    
  }
}
