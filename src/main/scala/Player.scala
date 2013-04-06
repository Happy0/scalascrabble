package scrabble

case class Player(
    val letters: List[Option[Letter]],
    val username: String,
    val score: Int){

}