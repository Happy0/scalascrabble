package scrabble

import scrabble.Tile

class Player(
    val tiles: List[Option[Tile]],
    username: String,
    val score: Int){

}