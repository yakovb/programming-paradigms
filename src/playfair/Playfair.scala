package playfair

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Created by Yakov Boglev on 11/01/2015.
 */
object Playfair {
  /**
   * Reads in user's keyword from the console
   * @return String of user input
   */
  def obtainKeywordFromUser(): String = {
    Console.in.readLine()
  }

  def obtainFileFromUser(name: String): Try[String] = {
    val str = Try(Source.fromFile(name).mkString)
    str match {
      case Success(s) =>
        val sl = s.toList
        if (sl.count((_: Char).isLetter) > 1) str
        else Failure(new IllegalArgumentException("The file doesn't have valid contents."))
      case Failure(s) => Failure(new IllegalArgumentException("The file doesn't exist or contains non-unicode characters."))
    }
  }

  /**
   * Checks user's keyword to ensure it has no non-letter characters, and is not empty
   * @param word user's keyword
   * @return Failure[IllegalArgumentException] if tests fail; Success[keyword as list of chars] otherwise.
   */
  def checkKeyword(word: String): Try[String]  = {
    val keyword = word.toList.filter(c => !c.isSpaceChar)
    if (keyword.isEmpty) Failure(new IllegalArgumentException("The supplied keyword is empty."))
    else if (keyword.exists(c => !c.isLetter)) Failure(new IllegalArgumentException("The supplied keyword contains non-letter characters."))
    else Success(word.filter(c => !c.isSpaceChar))
  }


}
