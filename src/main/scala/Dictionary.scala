package scrabble

import scala.collection.immutable.HashSet
import scala.io.Source
import java.io.File

case class Dictionary(dictionary: HashSet[String]) {
  
    /** Checks the dictionary for the validity of a word */
	def isValidWord(word: String) : Boolean = dictionary.contains(word)
	
	/** Returns a list of invalid words in the input list of words. Returns an empty list if there are none. */
	def invalidWords(words: List[String]) : List[String] =  words.filter(x => !isValidWord(x))
	
	override def toString = "Dictionary has " + dictionary.size + " letters."
}

object Dictionary {

  def load(languageFile: String): Dictionary = {
    val file = new File(getClass.getClassLoader.getResource(languageFile).getPath)
    val fileLines = Source.fromFile(file).getLines
    val dict: HashSet[String] = new HashSet() ++ fileLines.map(s => s.toUpperCase())
    Dictionary(dict)
  }

}


