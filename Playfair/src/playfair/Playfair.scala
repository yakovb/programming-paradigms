package playfair

import scala.util.{Try, Failure, Success}

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
