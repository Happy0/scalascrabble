package scrabble

import scala.collection.immutable.HashSet
import scala.io.Source

case class Dictionary(dictionary: HashSet[String]) {
  
	def isValidWord(word: String) = dictionary.contains(word)
}

object Dictionary {

  def load(languageFile: String): Dictionary = {
    val fileLines = Source.fromFile(languageFile).getLines
    val dict: HashSet[String] = new HashSet() ++ fileLines
    Dictionary(dict)
  }

  def main(args: Array[String]) {
    //@TODO: Find out how to make this a relative path / think about implications for deployment
    val dict = load("C:\\workspace\\Scala\\scalascrabble\\src\\Dict\\en.txt")
    println(dict.dictionary.size)
  }

}


